#!/bin/bash

# curl -vL http://climatedataapi.worldbank.org/climateweb/rest/v1/country/mavg/tas/1980/1999/GBR|jq

curl -L http://climatedataapi.worldbank.org/climateweb/rest/v1/country/cru/tas/year/GBR|jq
