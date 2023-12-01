let first str ~rex = (Pcre.extract ~rex str).(0)
