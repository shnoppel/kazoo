-module(gen_reducer).

-callback reduce() -> kz_term:json() | kz_term:proplist().
