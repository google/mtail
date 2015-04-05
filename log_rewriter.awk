BEGIN{
        now = systime()
        format = "%Y/%m/%d %H:%M:%S"
                
}
{
        split($1, DATE, "/")
        split($2, TIME, ":")
        $1 = ""
        $2 = ""
        t = mktime(DATE[1] " " DATE[2] " " DATE[3] " " TIME[1] " " TIME[2] " " TIME[3])
        if (delta == "") {
                delta = now - t
        }
        print strftime(format, t + delta) $0
}
