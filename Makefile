PROJECT = liberl
include erlang.mk
ERLC_OPTS = +debug_info +warn_export_all +warn_export_vars \
				+warn_shadow_vars +warn_obsolete_guard #-Werror +bin_opt_info +warn_missing_spec
