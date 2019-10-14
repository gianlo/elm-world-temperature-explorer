#!/bin/bash

# curl -L https://data.gov.uk/api/action/package_list | jq .result

REGIONAL_ANOMALIES=regional-climate-anomalies
# curl -L https://data.gov.uk/api/action/package_show?id=$REGIONAL_ANOMALIES|jq

# curl -L https://data.gov.uk/api/action/organization_list|jq

# curl -vL http://climatedataapi.worldbank.org/climateweb/rest/v1/country/mavg/tas/1980/1999/GBR|jq

curl -L http://climatedataapi.worldbank.org/climateweb/rest/v1/country/cru/tas/year/GBR|jq
