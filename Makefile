
REPO = images.canfar.net


# Change the following parameters for your project or set on the make command line
# e.g.  make dev PROJECT=uvickbos DEVNAME=iraf VERSION=0.1
PROJECT = uvickbos
DEVNAME = fossil
VERSION = 0.1

NAME = $(REPO)/$(PROJECT)/$(DEVNAME)

production: dependencies docker/Dockerfile
	docker build --target deploy -t $(NAME):$(VERSION) -f docker/Dockerfile .

deploy: production
	docker push $(NAME):$(VERSION)

dev: dependencies docker/Dockerfile
	docker build --target test -t $(NAME):$(VERSION) -f docker/Dockerfile .

dependencies: 

init:
	mkdir -p build

.PHONY: clean
clean:
	\rm -rf build
