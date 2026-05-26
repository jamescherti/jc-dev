#!/usr/bin/env python
"""A setuptools based setup module."""

from pathlib import Path

from setuptools import find_packages, setup

CURRENT_DIRECTORY = Path(__file__).parent.resolve()
LONG_DESCRIPTION = \
    (CURRENT_DIRECTORY / "README.md").read_text(encoding="utf-8")

setup(
    name="sampleproject",

    # Versions should comply with PEP 440:
    # https://www.python.org/dev/peps/pep-0440/
    version="1.0.0",

    description="A sample Python project",
    long_description=LONG_DESCRIPTION,
    long_description_content_type="text/markdown",
    url="https://github.com/GITHUBUSER/sampleproject",

    author="%USER%",  # Optional
    # author_email="author@example.com",  # Optional

    keywords="sample, setuptools, development",  # Optional

    # package_dir={"": "src"},  # Optional
    #
    # You can just specify package directories manually here if your project is
    # simple. Or you can use find_packages().
    #
    # Alternatively, if you just want to distribute a single Python file, use
    # the `py_modules` argument instead as follows, which will expect a file
    # called `my_module.py` to exist:
    #
    #   py_modules=["my_module"],
    #
    # packages=find_packages(where="src"),
    packages=find_packages(),

    python_requires=">=3.6, <4",

    # For an analysis of "install_requires" vs pip"s requirements files see:
    # https://packaging.python.org/en/latest/requirements.html
    # install_requires=['mypy', 'psutil', 'lxml'],

    # scripts=['monitor-xfconf-changes'],

    # For a list of valid classifiers, see https://pypi.org/classifiers/
    # classifiers=[  # Optional
    #     # How mature is this project? Common values are
    #     #   3 - Alpha
    #     #   4 - Beta
    #     #   5 - Production/Stable
    #     "Development Status :: 3 - Alpha",

    #     # Indicate who your project is intended for
    #     "Intended Audience :: Developers",
    #     "Topic :: Software Development :: Build Tools",

    #     # Pick your license as you wish
    #     "License :: OSI Approved :: MIT License",

    #     # Specify the Python versions you support here. In particular, ensure
    #     # that you indicate you support Python 3. These classifiers are *not*
    #     # checked by "pip install". See instead "python_requires" below.
    #     "Programming Language :: Python :: 3",
    #     "Programming Language :: Python :: 3.6",
    #     "Programming Language :: Python :: 3.7",
    #     "Programming Language :: Python :: 3.8",
    #     "Programming Language :: Python :: 3.9",
    #     "Programming Language :: Python :: 3 :: Only",
    # ],

    # List additional groups of dependencies here (e.g. development
    # dependencies). Users will be able to install these using the "extras"
    # syntax, for example:
    #
    #   $ pip install sampleproject[dev]
    #
    # Similar to `install_requires` above, these must be valid existing
    # projects.
    # extras_require={  # Optional
    #     "dev": ["check-manifest"],
    #     "test": ["coverage"],
    # },

    # If there are data files included in your packages that need to be
    # installed, specify them here.
    # package_data={  # Optional
    #     "sample": ["package_data.dat"],
    # },

    # Although "package_data" is the preferred approach, in some case you may
    # need to place data files outside of your packages. See:
    # http://docs.python.org/distutils/setupscript.html#installing-additional-files
    #
    # In this case, "data_file" will be installed into "<sys.prefix>/my_data"
    # data_files=[("my_data", ["data/data_file"])],  # Optional

    # To provide executable scripts, use entry points in preference to the
    # "scripts" keyword. Entry points provide cross-platform support and allow
    # `pip` to create the appropriate form of executable for the target
    # platform.
    #
    # For example, the following would provide a command called `sample` which
    # executes the function `main` from this package when invoked:
    # entry_points={  # Optional
    #     "console_scripts": [
    #         "sample=sample:main",
    #     ],
    # },

    # project_urls={  # Optional
    #     "Bug Reports": "https://github.com/pypa/sampleproject/issues",
    #     "Source": "https://github.com/pypa/sampleproject/",
    # },
)
