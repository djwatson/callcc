all: cloc

cloc:
	cloc  --by-file-by-lang gc.c list.c alloc_table.c types.c lib/bc.scm lib/expand.scm lib/library-manager.scm lib/match.scm lib/passes.scm lib/memory_layout.scm lib/runtime2.scm lib/str2num.scm lib/util.scm lib/gen-libraries.scm lib/qq.scm
