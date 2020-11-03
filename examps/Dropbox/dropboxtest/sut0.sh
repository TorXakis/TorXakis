cp testfile testdir
socat -v tcp-l:7890 system:'cd .; bash'
