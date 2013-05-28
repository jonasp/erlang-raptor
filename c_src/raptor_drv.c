/* raptor_drv.c */

#include <raptor.h>
#include <stdio.h>
#include <string.h>
#include "erl_driver.h"

/*
 * R15B changed several driver callbacks to use ErlDrvSizeT and
 * ErlDrvSSizeT typedefs instead of int.
 * This provides missing typedefs on older OTP versions.
 */
#if ERL_DRV_EXTENDED_MAJOR_VERSION < 2
typedef int ErlDrvSizeT;
typedef int ErlDrvSSizeT;
#endif

typedef struct {
	raptor_world* world;
    ErlDrvPort port;
    ErlDrvTermData port_term;
	ErlDrvMutex* mutex;
} driver_data;

static void message_handler(void *user_data, raptor_log_message *message)
{
	fprintf(stderr, "error code: %d\n\r", message->code);
	fprintf(stderr, "domain: %s\n\r", raptor_domain_get_label(message->domain));
	fprintf(stderr, "log level: %s\n\r", raptor_log_level_get_label(message->level));
	fprintf(stderr, "locator: \n", NULL);
	raptor_locator_print(message->locator,stderr);
	fprintf(stderr, "\n\r", NULL);
	fprintf(stderr, "message: %s\n\r", message->text);
}

static ErlDrvData raptor_drv_start(ErlDrvPort port, char *buff)
{
    driver_data* d = (driver_data*)driver_alloc(sizeof(driver_data));
    d->port = port;
    d->port_term = driver_mk_port(port);
	d->world = raptor_new_world();
	d->mutex= erl_drv_mutex_create("raptor_lock");
	raptor_world_set_log_handler(d->world, d, message_handler);
    return (ErlDrvData)d;
}

static void raptor_drv_stop(ErlDrvData handle)
{
	driver_data* d = (driver_data*)handle;
	erl_drv_mutex_destroy(d->mutex);
	raptor_free_world(d->world);
    driver_free((char*)handle);
}

static ErlDrvTermData term_type(raptor_term* term) {
	switch (term->type) {
	case RAPTOR_TERM_TYPE_UNKNOWN:
		return driver_mk_atom("unknown");
		break;
	case RAPTOR_TERM_TYPE_URI:
		return driver_mk_atom("uri");
		break;
	case RAPTOR_TERM_TYPE_LITERAL:
		return driver_mk_atom("literal");
		break;
	case RAPTOR_TERM_TYPE_BLANK:
		return driver_mk_atom("blank");
		break;
	}
	return -1;
}

static int send_data(driver_data *d, ErlDrvTermData *data, int len)
{
	erl_drv_mutex_lock(d->mutex);
/*
 * R16 deprecated driver_output and switched to er_drv_output_term.
 * erl_drv_output_term is only thread safe when the emulator with
 * SMP support is used. For safety we still lock.
 */
#if ((ERL_DRV_EXTENDED_MAJOR_VERSION == 2 && \
      ERL_DRV_EXTENDED_MINOR_VERSION == 1) || \
     ERL_DRV_EXTENDED_MAJOR_VERSION > 2)
	int result = erl_drv_output_term(d->port_term, data, len);
#else
	int result = driver_output_term(d->port, data, len);
#endif
	erl_drv_mutex_unlock(d->mutex);
	return result;
}

static void statement_handler(void *user_data, raptor_statement *statement)
{
	driver_data* d = (driver_data*)user_data;

	size_t slen, plen, olen;
	const char *s = (const char*)raptor_term_to_counted_string(statement->subject, &slen);
	const char *p = (const char*)raptor_term_to_counted_string(statement->predicate, &plen);
	const char *o = (const char*)raptor_term_to_counted_string(statement->object, &olen);

	ErlDrvTermData spec[] = {
				ERL_DRV_ATOM, driver_mk_atom("subject"),
					ERL_DRV_ATOM, term_type(statement->subject),
					ERL_DRV_STRING, (ErlDrvTermData)s, slen,
				ERL_DRV_TUPLE, 2,
			ERL_DRV_TUPLE, 2,
				ERL_DRV_ATOM, driver_mk_atom("predicate"),
					ERL_DRV_ATOM, term_type(statement->predicate),
					ERL_DRV_STRING, (ErlDrvTermData)p, plen,
				ERL_DRV_TUPLE, 2,
			ERL_DRV_TUPLE, 2,
				ERL_DRV_ATOM, driver_mk_atom("object"),
					ERL_DRV_ATOM, term_type(statement->object),
					ERL_DRV_STRING, (ErlDrvTermData)o, olen,
				ERL_DRV_TUPLE, 2,
			ERL_DRV_TUPLE, 2,
		ERL_DRV_TUPLE, 3
	};
	send_data(d, spec, sizeof(spec) / sizeof(spec[0]));

	raptor_free_memory((void*)s);
	raptor_free_memory((void*)p);
	raptor_free_memory((void*)o);
}

static char* decode_parser_name(char id)
{
	switch (id) {
		case 0: return NULL; break;
		case 1: return "rdfxml"; break;
		case 2: return "ntriples"; break;
		case 3: return "turtle"; break;
		case 4: return "trig"; break;
		case 5: return "rss-tag-soup"; break;
		case 6: return "grddl"; break;
		case 7: return "guess"; break;
		case 8: return "rdfa"; break;
		case 9: return "nquads"; break;
	}
	// TODO: Error handling
	return "";
}

static void raptor_drv_output(ErlDrvData handle, char *buff, 
		ErlDrvSizeT bufflen)
{
	driver_data* d = (driver_data*)handle;
	driver_mk_port(d->port);
	char fn = buff[0];
	fprintf(stderr, "call: %d\n\r", fn);
	if (fn == 1) {

		
	} else if (fn == 2) {
		char* parserName = decode_parser_name(buff[1]);
		fprintf(stderr, "format: %s\n\r", parserName);

		char* buffUri = NULL;
		if ((buffUri = malloc(bufflen-1)) == NULL) {
			// TODO: handle driver error
		}
		strncpy(buffUri, buff+2, bufflen-2);
		buffUri[bufflen-2] = '\0';
		fprintf(stderr, "uri: %s\n\r", buffUri);

		raptor_parser* rdf_parser = raptor_new_parser(d->world, parserName);
		raptor_parser_set_statement_handler(rdf_parser, d, statement_handler);
		raptor_uri* uri = raptor_new_uri(d->world, (unsigned char*)buffUri);

		int result = raptor_parser_parse_uri(rdf_parser, uri, NULL);

		if (result == 0) {

			ErlDrvTermData spec[] = {
				/*ERL_DRV_ATOM, driver_mk_atom("tcp"),*/
				/*ERL_DRV_PORT, driver_mk_port(drvport),*/
				/*ERL_DRV_INT, 100,*/
				/*ERL_DRV_ATOM, driver_mk_atom("length"),*/
				/*ERL_DRV_LIST, 2,*/
				/*ERL_DRV_TUPLE, 3,*/
				/*ERL_DRV_STRING, (ErlDrvTermData)buff, bufflen,*/
				/*ERL_DRV_ATOM, driver_mk_atom("data"),*/
				/*ERL_DRV_LIST, 1*/
				ERL_DRV_ATOM, driver_mk_atom("ok")
			};
			send_data(d, spec, sizeof(spec) / sizeof(spec[0]));
		} else {
			fprintf(stderr, "result: %d\n\r", result);
			ErlDrvTermData spec[] = {
					ERL_DRV_ATOM, driver_mk_atom("error"),
					ERL_DRV_INT, result,
				ERL_DRV_TUPLE, 2
			};
			send_data(d, spec, sizeof(spec) / sizeof(spec[0]));
		}


		raptor_free_parser(rdf_parser);
		raptor_free_uri(uri);

		free(buffUri);
		buffUri = NULL;
	}
}

ErlDrvEntry raptor_driver_entry = {
	NULL,			/* F_PTR init, called when driver is loaded */
	raptor_drv_start,		/* L_PTR start, called when port is opened */
	raptor_drv_stop,		/* F_PTR stop, called when port is closed */
	raptor_drv_output,		/* F_PTR output, called when erlang has sent */
	NULL,			/* F_PTR ready_input, called when input descriptor ready */
	NULL,			/* F_PTR ready_output, called when output descriptor ready */
	"raptor_drv",		/* char *driver_name, the argument to open_port */
	NULL,			/* F_PTR finish, called when unloaded */
	NULL,                       /* void *handle, Reserved by VM */
	NULL,			/* F_PTR control, port_command callback */
	NULL,			/* F_PTR timeout, reserved */
	NULL,			/* F_PTR outputv, reserved */
	NULL,                       /* F_PTR ready_async, only for async drivers */
	NULL,                       /* F_PTR flush, called when port is about 
								   to be closed, but there is data in driver 
								   queue */
	NULL,                       /* F_PTR call, much like control, sync call
								   to driver */
	NULL,                       /* F_PTR event, called when an event selected 
								   by driver_event() occurs. */
	ERL_DRV_EXTENDED_MARKER,    /* int extended marker, Should always be 
								   set to indicate driver versioning */
	ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be 
									   set to this value */
	ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be 
									   set to this value */
	0,                          /* int driver_flags, see documentation */
	NULL,                       /* void *handle2, reserved for VM use */
	NULL,                       /* F_PTR process_exit, called when a 
								   monitored process dies */
	NULL                        /* F_PTR stop_select, called to close an 
								   event object */
};

DRIVER_INIT(raptor_drv) /* must match name in driver_entry */
{
	return &raptor_driver_entry;
}
