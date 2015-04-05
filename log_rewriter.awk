BEGIN{
        now = systime()
        format = "%Y/%m/%d %H:%M:%S"
                
}
{
        split($1, DATE, "/")
        split($2, TIME, ":")
        t = mktime(DATE[1] " " DATE[2] " " DATE[3] " " TIME[1] " " TIME[2] " " TIME[3])
        if (delta == "") {
                delta = now - t
        }
        out = strftime(format, t + delta)
        for (i = 3; i <= NF; i++) {
                out = out OFS $i
        }
        print out
}
