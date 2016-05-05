@echo off

@rem get current and parent dir
set current=%cd%
pushd ..
set parent=%cd%
popd

@rem set dir to first param
set dir=%1
IF "%~1"=="" set dir=%current%
IF "%~1"=="." set dir=%current%
IF "%~1"==".." set dir=%parent%
nircmd exec show d:\Totalcmd\totalcmd.exe "%dir%"
