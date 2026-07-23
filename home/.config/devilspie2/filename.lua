--
-- Describe: Install the jc-dev development environment
--
-- Author: James Cherti
-- URL: https://github.com/jamescherti/jc-dev
--
-- Distributed under terms of the MIT license.
--
-- Copyright (C) 2004-2026 James Cherti
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--

-- Functions

function sleep(a)
    local sec = tonumber(os.clock() + a);
        while (os.clock() < sec) do
    end
end

monitor_width, monitor_height = get_screen_geometry();
function tile_center()
    local width = math.ceil(monitor_width / 2);
    set_window_geometry(math.ceil((monitor_width - width) / 2), 0, width, monitor_height);
    maximize_vertically();
end

function tile_left()
    set_window_geometry(0, 0, math.ceil(monitor_width / 2), monitor_height);
    maximize_vertically();
end

function tile_right_bigger()
    -- local xpos = math.ceil(monitor_width / 3)
    local xpos = math.ceil(monitor_width / 4)
    set_window_geometry(xpos, 0, monitor_width - xpos, monitor_height);
    maximize_vertically();
end

function tile_left_smaller()
    local width = math.ceil(monitor_width / 3)
    -- local width = math.ceil(monitor_width / 4)
    set_window_geometry(0, 0, width, monitor_height);
    maximize_vertically();
end

function tile_right()
    set_window_geometry(math.ceil(monitor_width / 2), 0, math.ceil(monitor_width / 2), monitor_height);
    maximize_vertically();
end

function set_change_workspace(num)
    set_window_workspace(num)
    change_workspace(num)
end

function center_window(width)
   set_window_geometry(math.ceil((monitor_width - width) / 2), 0, width, monitor_height);
   maximize_vertically()
end

-- Variables
local win_name = get_window_
local win_class = get_window_class();
local win_has_name = get_window_has_name();
local win_role = get_window_role();
local win_type = get_window_type();
sleep(0.2)  -- It seems to work better when we have to tile windows

local terminal_terminator = "Terminator"
local terminal_konsole = "Konsole"
local terminal_gnome = "Gnome-terminal"

local terminal_classes = {
    "Terminator",
    terminal_gnome,
    terminal_terminator,
    terminal_konsole
}

-- Settings

if win_has_name then
   ---------------------------------------------------------------------------
   -- Any desk
   ---------------------------------------------------------------------------
   -- if win_class == "Gvim" then
   --    tile_right_bigger();
   -- end

   ---------------------------------------------------------------------------
   -- First desk
   ---------------------------------------------------------------------------
   -- if (win_class == "Emacs") then
   --    sleep(2);
   --    -- set_change_workspace(1);
   --    tile_right_bigger();
   --    -- tile_center();
   --    undecorate_window();
   -- end

   if win_class == terminal_gnome or win_class == terminal_terminator or win_class == terminal_konsole then
      if not string.match(get_window_name(), "wcalc") then
         -- set_change_workspace(3);
         tile_center();
         undecorate_window();
         tile_left_smaller();
      end
   end

   if win_class == "Thunar" and win_type == "WINDOW_TYPE_NORMAL" then
      undecorate_window();
   end

   ---------------------------------------------------------------------------
   -- SECOND DESK
   ---------------------------------------------------------------------------
   if win_class == "libreoffice" then
      -- set_change_workspace(2);
      tile_left_bigger();
   end

   if (win_class == "Navigator" or win_class == "firefox-esr") and win_role == "browser" then
      -- set_change_workspace(2);
      tile_right_bigger();
   end

   if win_class == "thunderbird" or win_class == "thunderbird-default"  then
      -- Do not undecorate thunderbird because it does not work well
      -- Do not tile right
      -- set_change_workspace(3);
   end

   -- if win_class == "Virt-manager" then
   --    set_window_workspace(4);
   -- end
end

-- local debug_funcs = {
--     "get_window_name",
--     "get_window_has_name",
--     "get_application_name",
--     "get_window_geometry",
--     "get_window_client_geometry",
--     "get_window_is_maximized",
--     "get_window_is_maximized_vertically",
--     "get_window_is_maximized_horizontally",
--     "get_window_type",
--     "get_class_instance_name",
--     "get_window_role",
--     "get_window_xid",
--     "get_window_class",
--     "get_workspace_count",
--     "get_window_fullscreen",
--     "get_screen_geometry",
-- }
--
-- local debug_env = getfenv()
-- for i, f in ipairs(debug_funcs) do
--     debug_print(f .. "() == " .. tostring(debug_env[f]()))
-- end
--
-- debug_print("\n")


-- called when windows are closed
-- scripts_window_close = {"close.lua"}
