@echo off
xcopy /Y ".\*" "%USERPROFILE%\AppData\Local\Programs\insect\"


set SCRIPT="%TEMP%\%RANDOM%-%RANDOM%-%RANDOM%-%RANDOM%.vbs"

echo Set oWS = WScript.CreateObject("WScript.Shell") >> %SCRIPT%
echo sLinkFile = "%USERPROFILE%\Desktop\insect.lnk" >> %SCRIPT%
echo Set oLink = oWS.CreateShortcut(sLinkFile) >> %SCRIPT%
echo oLink.TargetPath = "%USERPROFILE%\AppData\Local\Programs\insect\insect.exe" >> %SCRIPT%
echo oLink.WorkingDirectory = "%USERPROFILE%\AppData\Local\Programs\insect\" >> %SCRIPT%
echo oLink.Save >> %SCRIPT%

cscript /nologo %SCRIPT%
del %SCRIPT%


set SCRIPT="%TEMP%\%RANDOM%-%RANDOM%-%RANDOM%-%RANDOM%.vbs"

echo Set oWS = WScript.CreateObject("WScript.Shell") >> %SCRIPT%
echo sLinkFile = "%USERPROFILE%\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\insect.lnk" >> %SCRIPT%
echo Set oLink = oWS.CreateShortcut(sLinkFile) >> %SCRIPT%
echo oLink.TargetPath = "%USERPROFILE%\AppData\Local\Programs\insect\insect.exe" >> %SCRIPT%
echo oLink.WorkingDirectory = "%USERPROFILE%\AppData\Local\Programs\insect\" >> %SCRIPT%
echo oLink.Save >> %SCRIPT%

cscript /nologo %SCRIPT%
del %SCRIPT%
