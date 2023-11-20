.PHONY: build test run

build:
	stack build 

test:
	stack test

run:
	stack run html-template-mig-example-app
	
## servers

run-hello-world:
	stack run hello-world-mig-example-app	

run-route-args:
	stack run route-args-mig-example-app

run-counter-client:
	stack run counter-mig-example-app

run-json-api:
	stack run json-api-mig-example-app

run-html:
	stack run html-mig-example-app

run-html-template:
	stack run html-template-mig-example-app

## clients

run-counter:
	stack run counter-client-mig-example-app

# Run it with launched "make run-hello-world" server
# It sends a clinet call for that server
run-hello-world-client:
	stack run hello-world-client-mig-example-app


# Run it with launched "make run-route-args" server
# It sends a clinet call for that server
run-route-args-client:
	stack run route-args-client-mig-example-app
