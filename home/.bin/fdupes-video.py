#!/usr/bin/env python
#
# Author: James Cherti
# URL: https://github.com/jamescherti/jc-dev
#
# Copyright (C) 2004-2026 James Cherti
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/>.
#
"""Find duplicate videos.

Requirements:
- ffmpeg
- ffprobe (Generally part of the ffmpeg package)
- md5sum

"""

import json
import logging
import os
import re
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor, as_completed
from copy import deepcopy
from multiprocessing import cpu_count
from pathlib import Path
from pprint import pformat
from typing import Union

from PIL import Image

from imagehash import average_hash

CACHE_DIR = Path("fdupes_video_cache").expanduser()
CACHE_FILE = CACHE_DIR / "fdupes_video_cache.json"
AMOUNT_PICTURES_TO_EXTRACT = 4
TIMEOUT_EXTRACT_IMAGE = 1200
FILE_READ_BYTES = 8 * 1024  # 64 KB block size
FD_THREADS = cpu_count() or 1
MAX_WORKERS = int(FD_THREADS / 2)  # max(cpu_count() - 6, 1)


class Md5sumError(Exception):
    """Failed to generate an md5sum."""


def gen_md5sum(filename: os.PathLike):
    """Return the MD5 sum of a file."""
    try:
        if Path(filename).is_file():
            output = subprocess.check_output(["md5sum", str(filename)],
                                             text=True)
            firstline = output.splitlines()[0]
            splitted_firstline = re.split(r"\s+", firstline)
            return splitted_firstline[0]
        # Add back python md5sum?
        #  and shutil.which("md5sum")
        # else:
        #     md5 = hashlib.md5()
        #     with open(filename, "rb") as fhandler:
        #         for chunk in iter(lambda: fhandler.read(FILE_READ_BYTES),
        #         b""):
        #             md5.update(chunk)
        #     return md5.hexdigest()
    except (subprocess.CalledProcessError,
            IndexError,
            FileNotFoundError) as err:
        raise Md5sumError(f"Failed to generate the md5sum of '{filename}'") \
            from err

    raise Md5sumError(f"Failed to generate the md5sum of '{filename}'")


class ExtractImageFromVideoError(Exception):
    """Error extracting the image from a video."""


class VideoInfo:
    # pylint: disable=too-many-positional-arguments
    def __init__(self,
                 md5sum: Union[None, str] = None,
                 duration: Union[None, str] = None,
                 duration_seconds: Union[None, float] = None,
                 frames_hashes: Union[None, dict[str, str]] = None,
                 path: os.PathLike = ""):
        self._logger = logging.getLogger(self.__class__.__name__)
        self.data: dict = {}

        self.data["paths"] = [str(path)] if path else []

        self.data["duration_seconds"] = 0
        self.data["duration"] = "00:00:00.000"
        self.data["md5sum"] = ""
        self.data["frames_hashes"] = {}
        self.data["failed"] = False

        if md5sum is not None:
            self.data["md5sum"] = md5sum

        if duration is not None:
            self.data["duration"] = duration

        if duration_seconds is not None:
            self.data["duration_seconds"] = duration_seconds

        if frames_hashes is not None:
            self.data["frames_hashes"] = deepcopy(frames_hashes)

    def to_dict(self) -> dict:
        return deepcopy(self.data)

    def update_from_dict(self, video_info_dict: dict):
        self.data.update(
            {"md5sum": video_info_dict["md5sum"],
             "duration": video_info_dict["duration"],
             "duration_seconds": video_info_dict["duration_seconds"],
             "frames_hashes": video_info_dict["frames_hashes"]})

    def update_video_info(self, filename: os.PathLike):
        filename = Path(filename).resolve()

        # self.data["md5sum"] = gen_md5sum(filename)

        ffprobe_result = self._parse_ffprobe_output(filename)
        self.data["duration"] = \
            str(ffprobe_result["format"]["duration"])

        self.data["duration_seconds"] = \
            self._sexagesimal_to_seconds(self.data["duration"])

        self._gen_frames_hashes(filename)

    def _gen_frames_hashes(self, filename: os.PathLike):
        """Generate the frames (images) hashes,"""
        position_to_add = \
            ((self["duration_seconds"] * 0.8) /     # Test 80%
             AMOUNT_PICTURES_TO_EXTRACT)
        position = self["duration_seconds"] * 0.1
        gen_number = 0
        while (gen_number < AMOUNT_PICTURES_TO_EXTRACT
               and position <= self["duration_seconds"]):
            gen_number += 1

            position_str = self._seconds_to_sexagesimal(position)
            self._logger.info("[%s] Gen frame %d: %s/%s",
                              filename,
                              gen_number,
                              position_str, self["duration"])

            ihash = self._video_to_imagehash(filename, position_str)
            self.data["frames_hashes"][position_str] = str(ihash)
            self._logger.info("[%s] Frame hash %d: %s", filename,
                              gen_number,
                              ihash)

            position += position_to_add

    def __getitem__(self, key: str):
        return self.data[key]

    def __repr__(self):
        return pformat(self.data)

    def equals(self, video_info2: "VideoInfo"):
        """Return True of the some frames in both videos are very similar."""
        # if self["md5sum"] == video_info2["md5sum"]:
        #     return (False, [])

        # if self["duration"] != video_info2["duration"]:
        #     return (False, [])

        checked_frames_positions = []
        different = False
        for frame_position, hash1 in self["frames_hashes"].items():
            checked_frames_positions.append(frame_position)

            try:
                hash2 = video_info2["frames_hashes"][frame_position]
            except KeyError:
                different = True
                break

            if hash1 != hash2:
                different = True
                break

        if not checked_frames_positions:
            different = True

        return (not different, checked_frames_positions)

    @staticmethod
    def _sexagesimal_to_seconds(sexagesimal_str: str) -> float:
        """Convert sexagesimal format HH:MM:SS.MICROSECONDS for seconds.

        >>> sexagesimal_to_seconds("")

        """
        parts = sexagesimal_str.split(":")
        if len(parts) == 3:
            hours = float(parts[0])
            minutes = float(parts[1])
            seconds = float(parts[2])
        elif len(parts) == 2:
            hours = 0.0
            minutes = float(parts[0])
            seconds = float(parts[1])
        else:
            raise ValueError("Invalid sexagesimal time format")

        total_seconds = hours * 3600 + minutes * 60 + seconds
        return float(total_seconds)

    @staticmethod
    def _seconds_to_sexagesimal(seconds: float):
        hours, remainder = divmod(float(seconds), 3600)
        minutes, seconds = divmod(remainder, 60)
        milliseconds = int((seconds % 1) * 1000)
        return (f"{int(hours):02d}:{int(minutes):02d}:"
                f"{int(seconds):02d}.{milliseconds:03d}")

    @staticmethod
    def _parse_ffprobe_output(file_path: os.PathLike):
        """Run the ffprobe command and capture its output as a byte string"""
        ffprobe_command = [
            'ffprobe',
            '-hide_banner',  # Hides banner, we do not need it to process file
            '-loglevel', 'quiet',  # Other messages are also not important
            '-print_format', 'json',
            '-sexagesimal',  # Time output format HH:MM:SS.MICROSECONDS
            '-show_format',  # Gives additional info about file format
            '-show_streams',  # Gives info about audio and video streams
            str(file_path),
        ]
        result = subprocess.run(ffprobe_command, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, text=True, check=True)

        # Parse the JSON output
        output_json = json.loads(result.stdout)
        return output_json

    @staticmethod
    def _extract_image_from_video(video_file: os.PathLike,
                                  image_file: os.PathLike,
                                  position: str):
        """Extract an image from a video using the FFmpeg utility.

        This function utilizes FFmpeg to extract a single frame from a video at
        the specified duration and save it as an image file.
        """
        image_file = Path(image_file)
        if image_file.is_file():
            # TODO: check if it is empty
            image_file.unlink()

        ffmpeg_command = ["ffmpeg",
                          "-nostdin",
                          "-i", str(video_file),
                          "-ss", str(position),
                          "-vframes", "1",
                          str(image_file)]

        try:
            subprocess.check_call(ffmpeg_command,
                                  # DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP
                                  # stdout=subprocess.DEVNULL,
                                  stderr=subprocess.DEVNULL,
                                  timeout=TIMEOUT_EXTRACT_IMAGE)
        except subprocess.CalledProcessError as err:
            raise ExtractImageFromVideoError(str(err)) from err

        if not image_file.is_file():
            raise ExtractImageFromVideoError(
                f"Failed to extract the image '{image_file}' "
                f"from the position {position} "
                f"of the video {video_file}")

    def _video_to_imagehash(self, video_file: os.PathLike, position: str):
        """Get the image hash of a specific frame.

        Arguments:
            video_file  Path to the video file.
            Position    Position at which to extract the image in the
                        'HH:MM:SS.mmm' format.

        """
        tmp_path = None
        try:
            _, tmp_path = tempfile.mkstemp(
                prefix="fdupes-video", suffix=".png")
            image_file = Path(tmp_path)

            self._extract_image_from_video(video_file=video_file,
                                           image_file=image_file,
                                           position=position)

            image = Image.open(image_file)
            try:
                ahash = average_hash(image)
            finally:
                del image
            return ahash
        finally:
            if tmp_path and Path(tmp_path).is_file():
                os.remove(tmp_path)


def save_video_info_cache(video_info_dict: dict[str, VideoInfo]):
    data: dict[str, dict[str, VideoInfo]] = {}
    data["video_info"] = {}

    for video_path, video_info in video_info_dict.items():
        data["video_info"][video_path] = video_info.to_dict()  # type: ignore

    json_content = json.dumps(data, indent=2)
    with open(CACHE_FILE, "w", encoding="utf-8") as fhandler:
        fhandler.write(json_content)


def load_video_info_cache():
    result = {}
    with open(CACHE_FILE, "r", encoding="utf-8") as fhandler:
        data = json.load(fhandler)
        # TODO: this should be part of VideoInfo
        for path, video_info_dict in data["video_info"].items():
            video_info = VideoInfo()
            video_info.update_from_dict(video_info_dict)
            result[path] = video_info

    return result


def generate_video_info(video_path, video_info_dict):
    save = False
    md5sum = gen_md5sum(video_path)
    logging.debug("Load file %s: %s", md5sum, video_path)

    video_info = None
    try:
        video_info = video_info_dict[md5sum]
    except KeyError:
        video_info = VideoInfo(md5sum, path=video_path)
        # video_info_dict[md5sum] = video_info
        save = True

    if video_info["failed"]:
        return video_path, md5sum, video_info, save

    # Update paths
    if video_info and str(video_path) not in video_info["paths"]:
        video_info["paths"].append(str(video_path))

    if not video_info["frames_hashes"]:
        backup_video_info = deepcopy(video_info)
        try:
            backup_video_info.update_video_info(video_path)
            save = True
        except subprocess.TimeoutExpired:
            backup_video_info["failed"] = True
            video_info = backup_video_info
        else:
            video_info = backup_video_info

    return video_path, md5sum, video_info, save


def parallel_md5sum(list_video_paths, video_info_dict):
    dict_md5sum = {}
    dict_paths = {}
    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        futures = []

        for video_path in list_video_paths:
            futures.append(executor.submit(generate_video_info,
                                           video_path,
                                           video_info_dict))

        try:
            for future in as_completed(futures):
                save = False
                try:
                    video_path, md5sum, video_info, save = future.result()

                    if video_info:
                        video_info_dict[video_info["md5sum"]] = video_info

                    if md5sum not in dict_md5sum:
                        dict_md5sum[md5sum] = set()

                    dict_md5sum[md5sum] |= \
                        set(video_info_dict[md5sum]["paths"])
                    dict_paths[video_path] = video_info_dict[md5sum]
                finally:
                    if save:
                        print("[SAVE JSON]", CACHE_FILE)
                        save_video_info_cache(video_info_dict)
        except KeyboardInterrupt:
            print("\nInterrupted.")
            sys.exit(1)

    return dict_md5sum, dict_paths, video_info_dict


# pylint: disable=too-many-statements
def main():
    """The command-line interface."""
    logging.basicConfig(level=logging.INFO, stream=sys.stdout,
                        format="%(asctime)s %(name)s: %(message)s")
    video_info_dict = {}

    if not CACHE_DIR.exists():
        CACHE_DIR.mkdir(exist_ok=True)

    try:
        video_info_dict = load_video_info_cache()
    except FileNotFoundError:
        video_info_dict = {}

    list_files = sys.argv[1:]

    # Resolve
    list_files = [Path(item).resolve() for item in list_files]

    # Generate md5sums
    _, dict_paths, video_info_dict = \
        parallel_md5sum(list_files, video_info_dict)

    for _, video_info in video_info_dict.items():
        if len(video_info["paths"]) > 1:
            print("Duplicate md5sum:")
            print("-----------------")
            for item in video_info["paths"]:
                print(item)
            print()

    # Find duplicates
    print()
    duplicates = {}
    done = {}
    for path1, video_info1 in dict_paths.items():
        for path2, video_info2 in dict_paths.items():
            if path1 == path2:
                continue

            # Never execute the same task again
            if path1 in done and path2 in done[path1]:
                continue

            if path2 in done and path1 in done[path2]:
                continue

            try:
                done[path1]
            except KeyError:
                done[path1] = set()

            done[path1].add(path2)

            try:
                done[path2]
            except KeyError:
                done[path2] = set()
            done[path2].add(path1)

            # Check if they are different
            videos_are_similar, _ = \
                video_info1.equals(video_info2)
            if videos_are_similar:
                # files1 = path1
                # files2 = path2

                key = "|".join(video_info1["frames_hashes"])
                try:
                    duplicates[key]
                except KeyError:
                    duplicates[key] = set()

                duplicates[key].add(path1)
                duplicates[key].add(path2)

                # if path1 in duplicates:
                #     try:
                #         duplicates[path1]
                #     except KeyError:
                #         duplicates[path1] = set()
                #
                #     duplicates[path1].add(path1)
                #     duplicates[path1].add(path2)
                # else:
                #     try:
                #         duplicates[path2]
                #     except KeyError:
                #         duplicates[path2] = set()
                #
                #     duplicates[path2].add(path1)
                #     duplicates[path2].add(path2)

                # print("Duplicate")
                # print("---------")
                # print(f"Duration: {video_info1['duration']}, "
                #       f"Tests: {checked_frames_positions}")
                # print(f"  - {files1}")
                # print(f"  - {files2}")
                # print()

    for _, dup in duplicates.items():
        print("Duplicates:")
        print("-----------")
        for dup_item in dup:
            print(dup_item)
        print()

    print("Finished.")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print()
