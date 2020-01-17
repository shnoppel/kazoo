%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_create_new_number_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

-export([db_dependant/0]).

knm_create_new_number_test_() ->
    knm_test_util:start_db(fun db_dependant/0).

db_dependant() ->
    [create_new_test_number()
    ,create_with_carrier()
    ,reseller_new_number()
    ,fail_new_number()
    ,create_new_available_number()
    ,create_existing_number()
    ,create_new_port_in()
    ,create_existing_in_service()
    ,create_dry_run()
    ,move_non_existing_mobile_number()
    ,create_non_existing_mobile_number()
    ,create_checks()
    ].

create_new_test_number() ->
    ?DEV_LOG("here 1"),
    Props = [{'auth_by', ?MASTER_ACCOUNT_ID}
            ,{'assign_to', ?RESELLER_ACCOUNT_ID}
            ,{'dry_run', 'false'}
            ],
    [PN] = knm_pipe:succeeded(knm_ops:create([?TEST_CREATE_NUM], Props)),
    JObj = knm_phone_number:to_public_json(PN),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify phone number is assigned to reseller account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number was authorized by master account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify new phone number database is properly set"
     ,?_assertEqual(<<"numbers%2F%2B1555">>, knm_phone_number:number_db(PN))
     }
    ,{"Verify new phone number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history is empty"
     ,?_assertEqual([], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the local carrier module is being used"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN))
     }
    ,{"Verify getting created field returns a number"
     ,?_assert(is_integer(knm_phone_number:created(PN)))
     }
    ,{"Verify the created field is stored as a number"
     ,?_assert(is_integer(kz_json:get_value([<<"_read_only">>, <<"created">>], JObj)))
     }
    ].

create_with_carrier() ->
    ?DEV_LOG("here 2"),
    CarrierModule = <<"blipblop">>,
    Props = [{'auth_by', ?MASTER_ACCOUNT_ID}
            ,{'assign_to', ?RESELLER_ACCOUNT_ID}
            ,{'dry_run', 'false'}
            ,{'module_name', CarrierModule}
            ],
    [PN] = knm_pipe:succeeded(knm_ops:create([?TEST_CREATE_NUM], Props)),
    JObj = knm_phone_number:to_public_json(PN),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify phone number is assigned to reseller account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number was authorized by master account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify new phone number database is properly set"
     ,?_assertEqual(<<"numbers%2F%2B1555">>, knm_phone_number:number_db(PN))
     }
    ,{"Verify new phone number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history is empty"
     ,?_assertEqual([], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the given carrier module is being used"
     ,?_assertEqual(CarrierModule, knm_phone_number:module_name(PN))
     }
    ,{"Verify no features were set"
     ,?_assertEqual([], knm_phone_number:features_list(PN))
     }
    ,{"Verify getting created field returns a number"
     ,?_assert(is_integer(knm_phone_number:created(PN)))
     }
    ,{"Verify the created field is stored as a number"
     ,?_assert(is_integer(kz_json:get_value([<<"_read_only">>, <<"created">>], JObj)))
     }
    ].

reseller_new_number() ->
    ?DEV_LOG("here 3"),
    Props = [{'auth_by', ?RESELLER_ACCOUNT_ID}
            ,{'assign_to', ?RESELLER_ACCOUNT_ID}
            ,{'dry_run', 'false'}
            ],
    [PN] = knm_pipe:succeeded(knm_ops:create([?TEST_CREATE_NUM], Props)),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify phone number is assigned to reseller account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number was authorized by master account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify new phone number database is properly set"
     ,?_assertEqual(<<"numbers%2F%2B1555">>, knm_phone_number:number_db(PN))
     }
    ,{"Verify new phone number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history is empty"
     ,?_assertEqual([], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the local carrier module is being used"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN))
     }
    ].

fail_new_number() ->
    ?DEV_LOG("here 4"),
    Props = [{'auth_by', ?RESELLER_ACCOUNT_ID}
            ,{'assign_to', ?RESELLER_ACCOUNT_ID}
            ,{'dry_run', 'false'}
            ,{<<"auth_by_account">>
             ,kzd_accounts:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'false')
             }
            ],
    [{?TEST_CREATE_NUM, Reason}] = knm_pipe:failed_to_proplist(knm_ops:create([?TEST_CREATE_NUM], Props)),
    [{"Verify that without the allow number additions the proper error is thrown"
     ,?_assertEqual(<<"forbidden">>, kz_json:get_value(<<"error">>, Reason))
     }
    ,{"Verify that without the allow number additions the proper message is thrown"
     ,?_assertEqual(<<"requestor is unauthorized to perform operation">>, kz_json:get_value(<<"message">>, Reason))
     }
    ].

create_new_available_number() ->
    ?DEV_LOG("here 5"),
    Props = [{'auth_by', ?MASTER_ACCOUNT_ID}
            ,{'assign_to', ?MASTER_ACCOUNT_ID}
            ,{'dry_run', 'false'}
            ,{state, ?NUMBER_STATE_AVAILABLE}
            ],
    [PN] = knm_pipe:succeeded(knm_ops:create([?TEST_CREATE_NUM], Props)),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify available number is unassigned"
     ,?_assertEqual(undefined, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number auth_by field was stored"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify new phone number database is properly set"
     ,?_assertEqual(<<"numbers%2F%2B1555">>, knm_phone_number:number_db(PN))
     }
    ,{"Verify new phone number is in AVAILABLE state"
     ,?_assertEqual(?NUMBER_STATE_AVAILABLE, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history is still empty"
     ,?_assertEqual([], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the local carrier module is being used"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN))
     }
    ].

create_existing_number() ->
    ?DEV_LOG("here 6"),
    Props = [{'auth_by', ?MASTER_ACCOUNT_ID}
            ,{'assign_to', ?RESELLER_ACCOUNT_ID}
            ,{'dry_run', 'false'}
            ],
    [PN] = knm_pipe:succeeded(knm_ops:create([?TEST_AVAILABLE_NUM], Props)),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify phone number is assigned to reseller account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number was authorized by master account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify new phone number database is properly set"
     ,?_assertEqual(<<"numbers%2F%2B1555">>, knm_phone_number:number_db(PN))
     }
    ,{"Verify new phone number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history is empty"
     ,?_assertEqual([], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the local carrier module is being used"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN))
     }
    ].

create_new_port_in() ->
    ?DEV_LOG("here 7"),
    Props = [{'auth_by', ?RESELLER_ACCOUNT_ID}
            ,{'assign_to', ?RESELLER_ACCOUNT_ID}
            ,{'dry_run', 'false'}
            ,{<<"auth_by_account">>
             ,kzd_accounts:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'false')
             }
            ,{'state', ?NUMBER_STATE_PORT_IN}
            ,{'module_name', ?CARRIER_LOCAL}
            ],
    [PN] = knm_pipe:succeeded(knm_ops:create([?TEST_CREATE_NUM], Props)),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify phone number is assigned to reseller account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number was authorized by master account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify new phone number database is properly set"
     ,?_assertEqual(<<"numbers%2F%2B1555">>, knm_phone_number:number_db(PN))
     }
    ,{"Verify new phone number is in PORT_IN state"
     ,?_assertEqual(?NUMBER_STATE_PORT_IN, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history is empty"
     ,?_assertEqual([], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the local carrier module is being used"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN))
     }
    ,{"Verify local number is not billable"
     ,?_assertEqual('false', knm_carriers:is_number_billable(PN))
     }
    ,{"Verify number is not marked as ported_in"
     ,?_assertEqual('false', knm_phone_number:ported_in(PN))
     }
    ].

create_existing_in_service() ->
    ?DEV_LOG("here 8"),
    Options = [{'assign_to', ?RESELLER_ACCOUNT_ID} | knm_options:default()],
    Resp = knm_numbers:create(?TEST_IN_SERVICE_NUM, Options),
    [{"Verifying that IN SERVICE numbers can't be created"
     ,?_assertMatch({'ok', [], _}, Resp)
     }
     | check_error_response(Resp, 409, <<"number_exists">>, ?TEST_IN_SERVICE_NUM)
    ].

create_dry_run() ->
    ?DEV_LOG("here 9"),
    Props = [{'auth_by', ?MASTER_ACCOUNT_ID}
            ,{'assign_to', ?RESELLER_ACCOUNT_ID}
            ,{'dry_run', 'true'}
            ],
    [?_assertMatch({'dry_run', _Quotes}, knm_numbers:create(?TEST_CREATE_NUM, Props))].

move_non_existing_mobile_number() ->
    ?DEV_LOG("here 10"),
    MobileField =
        kz_json:from_list(
          [{<<"provider">>, <<"tower-of-power">>}
          ,{<<"authorizing">>, kz_json:from_list([{<<"account-id">>, ?MASTER_ACCOUNT_ID}])}
          ,{<<"device-id">>, kz_binary:rand_hex(32)}
          ]),
    PublicFields = kz_json:from_list([{<<"mobile">>, MobileField}]),
    Props = [{'auth_by', ?MASTER_ACCOUNT_ID}
            ,{'dry_run', 'false'}
            ,{'public_fields', PublicFields}
            ,{'module_name', ?CARRIER_MDN}
            ],
    [{"Verify a non existing mdn cannot be moved to in_service"
     ,?_assertEqual({'ok', [], [{?TEST_CREATE_NUM, 'not_found'}]}, knm_numbers:move(?TEST_CREATE_NUM, ?RESELLER_ACCOUNT_ID, Props))
     }
    ].

create_non_existing_mobile_number() ->
    ?DEV_LOG("here 11"),
    MobileField =
        kz_json:from_list(
          [{<<"provider">>, <<"tower-of-power">>}
          ,{<<"authorizing">>, kz_json:from_list([{<<"account-id">>, ?MASTER_ACCOUNT_ID}])}
          ,{<<"device-id">>, kz_binary:rand_hex(32)}
          ]),
    PublicFields = kz_json:from_list([{<<"mobile">>, MobileField}]),
    Props = [{'auth_by', ?MASTER_ACCOUNT_ID}
            ,{'assign_to', ?RESELLER_ACCOUNT_ID}
            ,{'dry_run', 'false'}
            ,{'public_fields', PublicFields}
            ,{'module_name', ?CARRIER_MDN}
            ,{'mdn_run', 'true'}
            ],
    [PN] = knm_pipe:succeeded(knm_ops:create([?TEST_CREATE_NUM], Props)),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify phone number is assigned to reseller account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number was authorized by master account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify new phone number database is properly set"
     ,?_assertEqual(<<"numbers%2F%2B1555">>, knm_phone_number:number_db(PN))
     }
    ,{"Verify new phone number is in service state"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history is empty"
     ,?_assertEqual([], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the mdn carrier module is being used"
     ,?_assertEqual(?CARRIER_MDN, knm_phone_number:module_name(PN))
     }
    ,{"Verify the mobile public fields is exists"
     ,?_assert(kz_json:are_equal(MobileField
                                ,kz_json:get_value(<<"mobile">>, knm_phone_number:to_public_json(PN))
                                ))
     }
    ].

create_checks() ->
    create_available_checks()
        ++ load_existing_checks().

load_existing_checks() ->
    ?DEV_LOG("here 12"),
    {'ok', PN} = knm_phone_number:fetch(?TEST_AVAILABLE_NUM),
    [existing_in_state(knm_phone_number:set_state(PN, State), IsAllowed)
     || {State, IsAllowed} <- [{?NUMBER_STATE_AVAILABLE, 'true'}
                              ,{?NUMBER_STATE_DELETED, 'false'}
                              ,{?NUMBER_STATE_DISCOVERY, 'false'}
                              ,{?NUMBER_STATE_IN_SERVICE, 'false'}
                              ,{?NUMBER_STATE_PORT_IN, 'true'}
                              ,{?NUMBER_STATE_PORT_OUT, 'false'}
                              ,{?NUMBER_STATE_RELEASED, 'false'}
                              ,{?NUMBER_STATE_RESERVED, 'false'}
                              ]
    ].

existing_in_state(PN, 'false') ->
    ?DEV_LOG("here 13"),
    State = kz_term:to_list(knm_phone_number:state(PN)),
    Col0 = #{'failed' => #{}
            ,'options' => knm_options:default()
            ,'quotes' => 'undefined'
            ,'succeeded' => []
            ,'todo' => [PN]
            },
    Collection = knm_lib:ensure_can_load_to_create(Col0),
    [{lists:flatten(["Ensure number in ", State, " cannot be 'created'"])
     ,?_assert(kz_term:is_empty(knm_pipe:succeeded(Collection)))
     }
     | check_error_response(knm_pipe:failed(Collection), 409, <<"number_exists">>, ?TEST_AVAILABLE_NUM)
    ];

existing_in_state(PN, 'true') ->
    State = kz_term:to_list(knm_phone_number:state(PN)),
    Col0 = #{'failed' => #{}
            ,'options' => knm_options:default()
            ,'quotes' => 'undefined'
            ,'succeeded' => []
            ,'todo' => [PN]
            },
    [{lists:flatten(["Ensure number in ", State, " can be 'created'"])
     ,?_assertMatch([PN], knm_pipe:succeeded(knm_lib:ensure_can_load_to_create(Col0)))
     }
    ].

create_available_checks() ->
    [create_with_no_auth_by()
    ,create_with_disallowed_account()
    ,create_with_number_porting()
    ,create_new_number()
    ].

create_with_no_auth_by() ->
    Resp = knm_lib:ensure_can_create([]),
    [{"Ensure unauthorized error thrown when no auth_by supplied"
     ,?_assertMatch({'error', _}, Resp)
     }
     | check_error_response(Resp, 403, <<"forbidden">>)
    ].

create_with_disallowed_account() ->
    Resp = knm_lib:ensure_can_create(
             [{'auth_by', ?RESELLER_ACCOUNT_ID}
             ,{<<"auth_by_account">>
              ,kzd_accounts:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, 'false')
              }
             ]
            ),
    [{"Ensure unauthorized error when auth_by account isn't allowed to create numbers"
     ,?_assertMatch({'error', _}, Resp)
     }
     | check_error_response(Resp, 403, <<"forbidden">>)
    ].

create_with_number_porting() ->
    Collection = knm_pipe:new([], []),
    Col1 = knm_lib:ensure_numbers_are_not_porting([?TEST_NEW_PORT_NUM], Collection),
    [{"Ensure number_is_porting when creating a port request number"
     ,check_error_response(knm_pipe:failed_to_proplist(Col1), 400, <<"number_is_porting">>, ?TEST_NEW_PORT_NUM)
     }
    ].

check_error_response(E, Code, Error) ->
    check_error_response(E, Code, Error, 'undefined').

check_error_response(E, Code, Error, Cause) ->
    check_error_response(E, Code, Error, Cause, 'undefined').

check_error_response(#{}=E, Code, Error, Cause, Message) ->
    [{_, JObj}] = maps:to_list(E),
    check_error_response({'error', JObj}, Code, Error, Cause, Message);
check_error_response({'ok', [], [{_Num, JObj}]}, Code, Error, Cause, Message) ->
    check_error_response({'error', JObj}, Code, Error, Cause, Message);
check_error_response([{_Num, JObj}], Code, Error, Cause, Message) ->
    check_error_response({'error', JObj}, Code, Error, Cause, Message);
check_error_response({'error', JObj}, Code, Error, Cause, Message) ->
    validate_errors(JObj
                   ,[{Code, fun knm_errors:code/1, "Verify 'code' is set properly"}
                    ,{Error, fun knm_errors:error/1, "Verify 'error' is set properly"}
                    ,{Cause, fun knm_errors:cause/1, "Verify 'cause' is set properly"}
                    ,{Message, fun knm_errors:message/1, "Verify 'message' is set properly"}
                    ]).

validate_errors(JObj, Vs) ->
    validate_errors(JObj, Vs, []).

validate_errors(JObj, [{'undefined', _, _}|Vs], Tests) ->
    validate_errors(JObj, Vs, Tests);
validate_errors(_JObj, [], Tests) ->
    Tests;
validate_errors(JObj, [{V, F, L}|Vs], Tests) ->
    validate_errors(JObj
                   ,Vs
                   ,[{L, ?_assertEqual(V, F(JObj))} | Tests]
                   ).

create_new_number() ->
    {"Ensure success when auth_by account is allowed to create numbers"
     ,?_assertMatch({'ok',[{'state', ?NUMBER_STATE_IN_SERVICE}
                          ,{'assign_to', ?RESELLER_ACCOUNT_ID}
                          ,{'auth_by', ?RESELLER_ACCOUNT_ID}
                          ]
                    }
                    ,knm_lib:ensure_can_create([{'assign_to', ?RESELLER_ACCOUNT_ID}
                                                ,{'auth_by', ?RESELLER_ACCOUNT_ID}
                                               ]
                                              )
                   )
    }.
