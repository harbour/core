// Typical welcome message
// From \contrib\hboffice\tests
// ..\..\..\bin\win\mingw64\hbmk2 sheet1.prg -comp=mingw64

PROCEDURE Main()

    LOCAL c
    HBOFFICE_INIT()
    c := HBOFFICE_RGB(200, 100, 0)
    ? "Hello, world 2!"
    ? c
    HBOFFICE_FINISH()
    RETURN
