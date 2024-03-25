all : build pull start stop
.PHONY : all


build:
	docker build -t ghcr.io/geniusyield/dex-contracts-api:latest .

pull:
	docker pull ghcr.io/geniusyield/dex-contracts-api:latest

start:
	docker pull ghcr.io/geniusyield/dex-contracts-api:latest
	docker compose up

stop:
	docker compose down
