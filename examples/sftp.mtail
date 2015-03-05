# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

counter login_count by username
counter logout_count by username
counter bytes_read
counter files_read
counter bytes_written
counter files_written
counter user_bytes_read by username
counter user_files_read by username
counter user_bytes_written by username
counter user_files_written by username

/^(?P<date>\w+\s+\d+\s+\d+:\d+:\d+)\s+[\w\.-]+\s+sftp-server/ {
    strptime($date, "Jan _2 15:04:05")
    
    /session opened for local user (?P<username>\w+)/ {
        login_count[$username]++
    }
    
    /session closed for local user (?P<username>\w+)/ {
        logout_count[$username]++
    }
    
    /close "[^"]+" bytes read (?P<read>\d+) written (?P<written>\d+)/ {
        $read != 0 {
            bytes_read += $read
            files_read++ 
        }
        $written != 0 {
            bytes_written += $written
            files_written++
        }
    }

    /close "\/home\/(?P<username>[^\/]+)\/[^"]+"/ {
        $read != 0 {
            user_bytes_read[$username] += $read
            user_files_read[$username]++
        }
        $written != 0 {
            user_bytes_written[$username] += $written
            user_files_written[$username]++
        }
    }
}
