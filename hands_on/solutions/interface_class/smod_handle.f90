    SUBMODULE (mod_handle) smod_handle
      USE mod_file_handle, ONLY: file_handle
    CONTAINS
      MODULE PROCEDURE create_handle

        SELECT CASE (type)
        CASE ('file_handle')
          ALLOCATE (file_handle :: this)
        CASE DEFAULT
          STOP 'Error: unsupported extension of handle'
        END SELECT
      END PROCEDURE
    END SUBMODULE
