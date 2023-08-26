@echo off
setlocal

echo Starting MsBatchViewer
"Rscript.exe" -e "pkgload::load_all('.'); runApp(MsBatchViewer(), launch.browser = TRUE)" || goto stop

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
