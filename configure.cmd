@echo off
setlocal
setlocal enabledelayedexpansion

REM unfortunately, some issues tend to occur when using renv cache or the transactional / staging installation
REM so here, nuke the renv library before initializing to get a fresh start

echo Deleting the old renv library for rebuild.
"Rscript.exe" --no-init-file -e "unlink('./renv/library/', recursive = TRUE)" || goto stop

echo.
echo Checking renv. The project is initialized to:
"Rscript.exe" -e "message('RENV_PROJECT: ', Sys.getenv('RENV_PROJECT'))" || goto stop

echo.
echo Rebuilding the renv library.
"Rscript.exe" -e "renv::restore(rebuild = TRUE)" || goto stop

"Rscript.exe" -e "stopifnot(MsRawAccess::check_license_accepted())"
IF %ERRORLEVEL% NEQ 0 (
	echo.
	echo The license for the Thermo RawfileReader included with MsRawAccess needs to be check_license_accepted.
	"Rscript.exe" -e "MsRawAccess::print_license()"
	:ask_license
	echo Accept the license? ^(Y/N^)
	set /P INPUT=Type input: 
	If /I "!INPUT!"=="y" goto yes
	If /I "!INPUT!"=="n" goto no
	echo Incorrect input & goto ask_license
	:yes
	echo Registering that the license was accepted on this system.
	"Rscript.exe" -e "MsRawAccess::accept_license()"
)
:no

echo.
echo Finished.
:stop
set evalstatus=%ERRORLEVEL%
IF "%1" NEQ "--close_on_finish" (
	echo Press any key to close this window.
	pause >nul
)
endlocal
exit /b %evalstatus%
