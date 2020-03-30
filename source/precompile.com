$!*************************************************
$!
$! Command procedure to modify some files...
$!
$!*************************************************
$!
$ OPEN/WRITE TOTO AAA.DAT
$ WRITE TOTO "ACCEPT 10,@@"
$ WRITE TOTO "READ(5,10) @@"
$ WRITE TOTO "''P1'*.FOR"
$ CLOSE TOTO
$ TYPE AAA.DAT
$ ASSIGN/USER AAA.DAT SYS$INPUT
$ RUN EXE:PRECOMPILER
$ DEL AAA.DAT.*
$ PU
$!
$ OPEN/WRITE TOTO AAA.DAT
$ WRITE TOTO "ACCEPT *,@@"
$ WRITE TOTO "READ(5,*) @@"
$ WRITE TOTO "''P1'*.FOR"
$ CLOSE TOTO
$ TYPE AAA.DAT
$ ASSIGN/USER AAA.DAT SYS$INPUT
$ RUN EXE:PRECOMPILER
$ DEL AAA.DAT.*
$ EXIT
