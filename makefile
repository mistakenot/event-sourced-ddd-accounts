up:
	docker-compose down && docker-compose up -d --build

psql:
	psql -h localhost -p 5432 -U postgres

logs:
	docker-compose logs