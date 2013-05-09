/* raptor_drv.c */

#include <raptor.h>
#include <stdio.h>
#include "erl_driver.h"

typedef struct {
    ErlDrvPort port;
	raptor_world* world;
} example_data;

static ErlDrvData raptor_drv_start(ErlDrvPort port, char *buff)
{
    example_data* d = (example_data*)driver_alloc(sizeof(example_data));
    d->port = port;
	d->world = raptor_new_world();
    return (ErlDrvData)d;
}

static void raptor_drv_stop(ErlDrvData handle)
{
	example_data* d = (example_data*)handle;
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

static void statement_handler(void *user_data, raptor_statement *statement)
{
	example_data* d = (example_data*)user_data;

	size_t slen, plen, olen;
	const char *s = (const char*)raptor_term_to_counted_string(statement->subject, &slen);
	const char *p = (const char*)raptor_term_to_counted_string(statement->predicate, &plen);
	const char *o = (const char*)raptor_term_to_counted_string(statement->object, &olen);

	ErlDrvPort drvport = d->port;
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
	/* depend on deprecated call because homebrew is still on R15 */
	/*erl_drv_output_term(driver_mk_port(drvport), spec, sizeof(spec) / sizeof(spec[0]));*/
	driver_output_term(drvport, spec, sizeof(spec) / sizeof(spec[0]));

	raptor_free_memory((void*)s);
	raptor_free_memory((void*)p);
	raptor_free_memory((void*)o);
}

static void raptor_drv_output(ErlDrvData handle, char *buff, 
		ErlDrvSizeT bufflen)
{
	example_data* d = (example_data*)handle;
	char fn = buff[0], res;
	fprintf(stderr, "call: %d\n\r", fn);
	if (fn == 1) {

		
	} else if (fn = 2) {
		char arg[bufflen-1];
		strcpy(arg,buff+1);
		
		raptor_parser* rdf_parser = raptor_new_parser(d->world, "turtle");
		raptor_parser_set_statement_handler(rdf_parser, handle, statement_handler);
		raptor_uri* uri = raptor_new_uri(d->world, arg);
		raptor_uri* base_uri = raptor_uri_copy(uri);

		raptor_parser_parse_uri(rdf_parser, uri, base_uri);

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
			ERL_DRV_ATOM, driver_mk_atom("ok"),
		};

		driver_output_term(d->port, spec, sizeof(spec) / sizeof(spec[0]));
		/* depend on deprecated call because homebrew is still on R15 */
		/*erl_drv_output_term(driver_mk_port(drvport), spec, sizeof(spec) / sizeof(spec[0]));*/

		raptor_free_parser(rdf_parser);
		raptor_free_uri(base_uri);
		raptor_free_uri(uri);
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
