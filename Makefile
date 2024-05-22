all : build pull start stop
.PHONY : all


build:
	docker build -t ghcr.io/geniusyield/dex-contracts-api .

pull:
	docker compose pull

start:
	docker compose up -d --remove-orphans

start-kupo:
	docker-compose -f docker-compose-kupo.yml up -d

node-logs:
	docker compose logs -f node

logs:
	docker compose logs -f

stop:
	docker compose down

test:
	@[ ! -f .env ] || export $(grep -v 'SERVER_API_KEY' .env | xargs) >/dev/null 2>&1
	@curl -H "api-key: ${SERVER_API_KEY}" http://localhost:8082/v0/settings && echo
