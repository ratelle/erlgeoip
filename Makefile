REBAR=./rebar
DOWNLOAD_PATH=http://geolite.maxmind.com/download/geoip/database/
DB_DIR=/usr/local/share/GeoIp/

all: deps databases compile xref

deps: databases
	@$(REBAR) update-deps
	@$(REBAR) get-deps

databases:
	curl -z "$(DB_DIR)GeoIP.dat.gz" $(DOWNLOAD_PATH)GeoLiteCountry/GeoIP.dat.gz -o "$(DB_DIR)GeoIP.dat.gz" --create-dirs
	gunzip -c "$(DB_DIR)GeoIP.dat.gz" > "$(DB_DIR)GeoIP.dat"
	curl -z "$(DB_DIR)GeoIPCity.dat.gz" $(DOWNLOAD_PATH)GeoLiteCity.dat.gz -o "$(DB_DIR)GeoIPCity.dat.gz" --create-dirs
	gunzip -c "$(DB_DIR)GeoIPCity.dat.gz" >  "$(DB_DIR)GeoIPCity.dat"
	curl -z "$(DB_DIR)GeoIPASNum.dat.gz" $(DOWNLOAD_PATH)asnum/GeoIPASNum.dat.gz -o "$(DB_DIR)GeoIPASNum.dat.gz" --create-dirs
	gunzip -c "$(DB_DIR)GeoIPASNum.dat.gz" > "$(DB_DIR)GeoIPASNum.dat"
	curl -z "$(DB_DIR)GeoIPv6.dat.gz" $(DOWNLOAD_PATH)GeoIPv6.dat.gz -o "$(DB_DIR)GeoIPv6.dat.gz" --create-dirs
	gunzip -c "$(DB_DIR)GeoIPv6.dat.gz" > "$(DB_DIR)GeoIPv6.dat"
	curl -z "$(DB_DIR)GeoIPCityv6.dat.gz" $(DOWNLOAD_PATH)GeoLiteCityv6-beta/GeoLiteCityv6.dat.gz -o "$(DB_DIR)GeoIPCityv6.dat.gz" --create-dirs
	gunzip -c "$(DB_DIR)GeoIPCityv6.dat.gz" > "$(DB_DIR)GeoIPCityv6.dat"
	curl -z "$(DB_DIR)GeoIPASNumv6.dat.gz" $(DOWNLOAD_PATH)asnum/GeoIPASNumv6.dat.gz -o "$(DB_DIR)GeoIPASNumv6.dat.gz" --create-dirs
	gunzip -c "$(DB_DIR)GeoIPASNumv6.dat.gz" > "$(DB_DIR)GeoIPASNumv6.dat"


xref:
	@$(REBAR) skip_deps=true xref

compile:
	$(REBAR) compile

test:  compile
	@$(REBAR) skip_deps=true ct

clean:
	@rm -rf deps
	@$(REBAR) clean

.PHONY: all deps compile clean

