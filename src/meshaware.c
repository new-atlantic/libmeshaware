#include "meshaware.h"

int maw_determine_mesh_protocol(maw_mesh_protocol *protocol) {
	protocol->name = batman_adv;
	protocol->version = (char*) '1';
	return 0;
}