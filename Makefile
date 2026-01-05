.PHONY: test repl

test:
	ros -Q script scripts/test.ros

repl:
	ros -Q script scripts/repl.ros
