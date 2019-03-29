DNVGL ANSYS lib
===============

Drop in FORTRAN code to help writing ANSYS UPF extension providing
MODULES to ensure correct parameters for lots of UPF functions and
constants. Also provides convenient wrappers for some ANSYS features

  - `ezTrackBegin`, `ezTrackEnd`: wrappers around `TracBegin` and
    `TrackEnd`. `TrackBegin` takes a string or a string or a string
    and a number, the latter interpreted as filen name and line
    number. `ezTrackEnd` takes no argument and ends the track started
    with the last `ezTrackBegin` call.

    ```fortran
        CALL ezTrackBegin(__FILE__, __LINE__)
        CALL ezTrackEnd()

        CALL ezTrackBegin(__FILE__, __LINE__)
        CALL ezTrackBegin("Point of interest")
        CALL ezTrackEnd()
        CALL ezTrackEnd()
    ```

  - `ezTrackMark`: Just `TrackBegin` directly fllowed by `TrackEnd`.

  - `ezRunCommand`: eases use of `RunCommand`

  - `ezBanner`: Informational output

  - `get`: calls `*GET` with differen variable types

etc.

Also some Python code is included to set up the build settings for
various ANSYS versions.
