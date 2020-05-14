h_api.pl:	hypothesis-v1.yaml
	../openapi/bin/swi-openapi --module --client=$@ hypothesis-v1.yaml
