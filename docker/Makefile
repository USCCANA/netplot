build:
	docker build -t usccana/netplot:release -f Dockerfile . && \
		docker build -t usccana/netplot:devel -f Dockerfile.dev .
push:
	docker push usccana/netplot:release && \
		docker push usccana/netplot:devel
