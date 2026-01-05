.PHONY: test repl

test:
	ros -Q run -l scripts/test.ros

repl:
	ros -Q run -l scripts/repl.ros
