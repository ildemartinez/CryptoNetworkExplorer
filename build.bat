@echo off
if exist "Win32" ( 
	echo Borrando directorio Win32
	rmdir Win32 /s/ q 
)

if exist "Win64" ( 
	echo Borrando directorio Win32
	rmdir Win64 /s/ q
)

echo Estableciendo variables de entorno
call "c:\Program Files (x86)\Embarcadero\Studio\21.0\bin\rsvars.bat"

echo Compilando
msbuild /t:clean /t:build /p:Config=Release;Platform=Win32 vicyne.dproj

if exist .\Win32\Release\vicyne.exe (goto L1)
echo "Error compiling"
pause
exit

:L1
echo --------------------
echo Comprimiendo 
echo --------------------
upx.exe Win32\Release\vicyne.exe
cd Win32\Release

echo --------------------
echo Zipping
echo --------------------
"C:\Program Files\7-Zip\7z.exe" a vicyne.zip vicyne.exe 

pause

