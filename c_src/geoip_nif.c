#include <erl_nif.h>
#include <GeoIP.h>
#include <GeoIPCity.h>

#define	IS_KNOWN(str)	(((str) != NULL) && (strcmp((str), "--") != 0) && \
			 (strcmp((str), "N/A") != 0) &&			\
			 (strcmp((str), "(null)") != 0) &&		\
			 (strcmp((str), "null") != 0) &&		\
			 (strcmp((str), "Unknown") != 0))

#define GEOIP_MODE GEOIP_MEMORY_CACHE

static const char *netspeeds[] = {"--", "Dialup", "Cable/DSL", "Corporate"};

static GeoIP *gip_country_edition = NULL;
static GeoIP *gip_region_edition = NULL;
static GeoIP *gip_city_edition = NULL;
static GeoIP *gip_org_edition = NULL;
static GeoIP *gip_isp_edition = NULL;
static GeoIP *gip_netspeed_edition = NULL;
static GeoIP *gip_netspeed_edition_rev1 = NULL;

static int
on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info)
{

    if (GeoIP_db_avail(GEOIP_CITY_EDITION_REV1))
	gip_city_edition = GeoIP_open_type(GEOIP_CITY_EDITION_REV1,
						GEOIP_MODE);
    else if (GeoIP_db_avail(GEOIP_CITY_EDITION_REV0))
	gip_city_edition = GeoIP_open_type(GEOIP_CITY_EDITION_REV0,
					   GEOIP_MODE);
    else if (GeoIP_db_avail(GEOIP_REGION_EDITION_REV1))
	gip_region_edition = GeoIP_open_type(GEOIP_REGION_EDITION_REV1,
					     GEOIP_MODE);
    else if (GeoIP_db_avail(GEOIP_REGION_EDITION_REV0))
	gip_region_edition = GeoIP_open_type(GEOIP_REGION_EDITION_REV0,
					     GEOIP_MODE);
    else if (GeoIP_db_avail(GEOIP_COUNTRY_EDITION))
	gip_country_edition = GeoIP_open_type(GEOIP_COUNTRY_EDITION,
					      GEOIP_MODE);

    if (GeoIP_db_avail(GEOIP_ORG_EDITION))
	gip_org_edition = GeoIP_open_type(GEOIP_ORG_EDITION,
					  GEOIP_MODE);

    if (GeoIP_db_avail(GEOIP_ISP_EDITION))
	gip_isp_edition = GeoIP_open_type(GEOIP_ISP_EDITION,
					  GEOIP_MODE);

    if (GeoIP_db_avail(GEOIP_NETSPEED_EDITION_REV1))
	gip_netspeed_edition_rev1 = GeoIP_open_type(GEOIP_NETSPEED_EDITION_REV1,
						    GEOIP_MODE);
    else if (GeoIP_db_avail(GEOIP_NETSPEED_EDITION))
	gip_netspeed_edition = GeoIP_open_type(GEOIP_NETSPEED_EDITION,
					       GEOIP_MODE);

    int i;
    GeoIP *gis[] = {
	gip_country_edition,
	gip_region_edition,
	gip_city_edition,
	gip_org_edition,
	gip_isp_edition,
	gip_netspeed_edition,
	gip_netspeed_edition_rev1
    };

    for (i = 0; i < sizeof(gis)/sizeof(GeoIP*); i++) {
	if (gis[i] != NULL)
	    GeoIP_set_charset(gis[i], GEOIP_CHARSET_UTF8);
    }

    return 0;
}

static ERL_NIF_TERM
make_atom(ErlNifEnv *env, const char *name)
{
    ERL_NIF_TERM ret;

    if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

static ERL_NIF_TERM
make_binary_string(ErlNifEnv *env, const char *s)
{
    ErlNifBinary b;
    int n;

    n = strlen(s);
    enif_alloc_binary(n, &b);
    memcpy(b.data, s, n);
    b.size = n;
    return enif_make_binary(env, &b);
}

static ERL_NIF_TERM
make_geoip_binary_string(ErlNifEnv *env, const char *s)
{
    if (!IS_KNOWN(s))
	s = "";

    return make_binary_string(env, s);
}

static ERL_NIF_TERM
make_error(ErlNifEnv *env, const char *mesg)
{
    ERL_NIF_TERM error = make_atom(env, "error");
    return enif_make_tuple2(env, error, make_atom(env, mesg));
}

static ERL_NIF_TERM
geo_lookup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;
    char *ip;
    GeoIPLookup gl;

    const char *country_code = NULL;
    const char *country_code3 = NULL;
    const char *country_name = NULL;
    GeoIPRegion *region = NULL;
    const char *region_name = NULL;
    GeoIPRecord *gir = NULL;
    const char *city_name = NULL;
    const char *postal_code = NULL;
    double latitude = 0.0;
    double longitude = 0.0;
    int area_code = 0;
    int dma_code = 0;
    char *organization_name = NULL;
    char *isp_name = NULL;
    char *netspeed_rev1 = NULL;
    const char *netspeed = NULL;

    if (argc != 1) {
        return enif_make_badarg(env);
    }
    if (!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    ip = (char *) enif_alloc(input.size+1);

    if (!ip) {
        return make_error(env, "out_of_memory");
    }
    memcpy(ip, input.data, input.size);
    ip[input.size] = '\0';

    if (gip_city_edition != NULL) {
	gir = GeoIP_record_by_addr(gip_city_edition, ip);
	if (gir != NULL) {
	    country_code = gir->country_code;
	    country_code3 = gir->country_code3;
	    country_name = gir->country_name;
	    region_name = gir->region;
	    city_name = gir->city;
	    postal_code = gir->postal_code;
	    latitude = gir->latitude;
	    longitude = gir->longitude;
	    area_code = gir->area_code;
	    dma_code = gir->dma_code;
	}
    }
    /* Fallback when city editions are not available */
    else {
	int country_id = 0;
	GeoIP *current_gi = NULL;
	if (gip_region_edition != NULL) {
	    region = GeoIP_region_by_addr_gl(gip_region_edition, ip, &gl);
	    current_gi = gip_region_edition;
	    country_id = GeoIP_id_by_code(region->country_code);
	    region_name = region->region;
	}
	else if (gip_country_edition != NULL) {
	    country_id = GeoIP_id_by_addr_gl(gip_country_edition, ip, &gl);
	    current_gi = gip_country_edition;
	}

	if (country_id > 0) {
	    country_code = GeoIP_code_by_id(country_id);
	    country_code3 = GeoIP_code3_by_id(country_id);
	    country_name = GeoIP_country_name_by_id(current_gi, country_id);
	}
    }

    if (gip_org_edition != NULL) {
	organization_name = GeoIP_name_by_addr_gl(gip_org_edition, ip, &gl);
    }

    if (gip_isp_edition != NULL) {
	isp_name = GeoIP_name_by_addr_gl(gip_isp_edition, ip, &gl);
    }

    if (gip_netspeed_edition_rev1 != NULL) {
	netspeed_rev1 = GeoIP_name_by_addr_gl(gip_netspeed_edition_rev1, ip, &gl);
	netspeed = netspeed_rev1;
    }
    else if (gip_netspeed_edition != NULL) {
	GeoIPNetspeedValues netspeed_id = GeoIP_id_by_addr_gl(gip_netspeed_edition, ip, &gl);
	netspeed = netspeeds[(int)netspeed_id];
    }

    ERL_NIF_TERM erl_geoip;
    ERL_NIF_TERM erl_geoip_header = make_atom(env, "geoip");
    ERL_NIF_TERM erl_country_code = make_geoip_binary_string(env, country_code);
    ERL_NIF_TERM erl_country_code3 = make_geoip_binary_string(env, country_code3);
    ERL_NIF_TERM erl_country_name = make_geoip_binary_string(env, country_name);
    ERL_NIF_TERM erl_region_name = make_geoip_binary_string(env, region_name);
    ERL_NIF_TERM erl_city_name = make_geoip_binary_string(env, city_name);
    ERL_NIF_TERM erl_postal_code = make_geoip_binary_string(env, postal_code);
    ERL_NIF_TERM erl_latitude = enif_make_double(env, latitude);
    ERL_NIF_TERM erl_longitude = enif_make_double(env, longitude);
    ERL_NIF_TERM erl_area_code = enif_make_int(env, area_code);
    ERL_NIF_TERM erl_dma_code = enif_make_int(env, dma_code);
    ERL_NIF_TERM erl_organization_name = make_geoip_binary_string(env, organization_name);
    ERL_NIF_TERM erl_isp_name = make_geoip_binary_string(env, isp_name);
    ERL_NIF_TERM erl_netspeed = make_geoip_binary_string(env, netspeed);

    const ERL_NIF_TERM terms[] = {
	erl_geoip_header,
	erl_country_code,
	erl_country_code3,
	erl_country_name,
	erl_region_name,
	erl_city_name,
	erl_postal_code,
	erl_latitude,
	erl_longitude,
	erl_area_code,
	erl_dma_code,
	erl_organization_name,
	erl_isp_name,
	erl_netspeed
    };

    erl_geoip = enif_make_tuple_from_array (env, terms, sizeof(terms)/sizeof(ERL_NIF_TERM));

    if (region != NULL)
	GeoIPRegion_delete(region);
    if (gir != NULL)
	GeoIPRecord_delete(gir);
    if (organization_name != NULL)
	free(organization_name);
    if (isp_name != NULL)
	free(isp_name);
    if (netspeed_rev1 != NULL)
	free(netspeed_rev1);
    enif_free(ip);

    return enif_make_tuple2(env, make_atom(env, "ok"), erl_geoip);

}

static ErlNifFunc nif_functions[] = {
    {"lookup", 1, geo_lookup}
};

ERL_NIF_INIT(erlgeoip, nif_functions, &on_load, NULL, NULL, NULL);
