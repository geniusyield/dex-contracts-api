all : build pull start stop
.PHONY : all


build:
	docker build -t ghcr.io/geniusyield/dex-contracts-api .

pull:
	docker compose pull

start:
	docker compose up --no-recreate

stop:
	docker compose down
