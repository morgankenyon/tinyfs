# TinyFS.Cli

This holds the CLI functionality for TinyFS.

Please see the [TinyFS](https://github.com/morgankenyon/tinyfs) project for better instructions on how to setup your local machine.

## Commands
* Generate new package - `run ./pack.ps1`
* Install as local tool from source - `dotnet tool install --local --add-source .\bin\Release\ TinyFS.Cli`
* Install as global tool from source - `dotnet tool install --global --add-source .\bin\Release\ TinyFS.Cli`
  * If not running from `/cli` folder, update the `.\bin\Release` to wherever you *.nupkg is located.
* Install from nuget - `dotnet tool install --global TinyFS.Cli --version 0.0.1`
* Uninstall tool - `dotnet tool uninstall -g TinyFS.Cli`