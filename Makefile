all : build pull start stop
.PHONY : all


build:
	docker build -t ghcr.io/geniusyield/dex-contracts-api:preview .

pull:
	docker pull ghcr.io/geniusyield/dex-contracts-api:preview

start:
	docker pull ghcr.io/geniusyield/dex-contracts-api:preview
	docker compose up

stop:
	docker compose down
