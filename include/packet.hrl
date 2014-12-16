%% Author: Peter
%% Created: Dec 15, 2008
%% Commands in Packets
-include("common.hrl").

-define(CMD_POLICYREQUEST, <<60,112,111,108,105,99,121,45,102,105,108,101,45,114,101,113,117,101,115,116,47,62,0>>).
-define(CMD_POLICY, <<"<cross-domain-policy>\n<allow-access-from domain=\"*\" to-ports=\"*\" />\n</cross-domain-policy>", 0>>).

-define(CMD_BAD, 255).
-record(bad, {cmd,
              error = ?ERR_UNKNOWN}).

-define(CMD_LOGIN, 1).
-record(login, {name,
                pass}).

-record(login2, {cmd,
                 username,
                 password}).

-define(CMD_LOGOUT, 2).
-record(logout, {}).

-define(CMD_CLOCKSYNC, 3).
-define(CMD_CLIENTREADY, 4).

-define(CMD_PLAYER_ID, 5).
-record(player_id, {id}).

-define(CMD_INFO_KINGDOM, 6).
-record(info_kingdom, {id,
                       name,
                       gold,
                       item_recipes,
                       unit_recipes}).

-define(CMD_CHAT_MESSAGE, 7).
-record(chat_message, {player_id,
                       player_name,
                       message}).

-define(CMD_SUCCESS, 20).
-record(success, {type,
                  id}).

-define(CMD_EXPLORED_MAP, 39).
-record(map, {tiles}).

-define(CMD_REMOVE_TASK, 131).
-record(remove_task, {city_id,
                      assignment_id}).

-define(CMD_TRANSFER_ITEM, 150).
-record(transfer_item, {item_id,
                        source_id,
                        source_type,
                        target_id,
                        target_type}).

-define(CMD_DELETE_ITEM, 151).
-record(delete_item, {item_id}).

-define (CMD_TRANSFER_UNIT, 155).
-record(transfer_unit, {unit_id,
                        source_id,
                        source_type,
                        target_id,
                        target_type}).

-define(CMD_CREATE_SELL_ORDER, 170).
-record(create_sell_order, {item_id,
                            price}).

-define(CMD_CREATE_BUY_ORDER, 171).
-record(create_buy_order, {city_id,
                           item_type,
                           volume,
                           price}).

-define(CMD_FILL_SELL_ORDER, 172).
-record(fill_sell_order, {order_id,
                          volume}).


-define(CMD_FILL_BUY_ORDER, 173).
-record(fill_buy_order, {order_id,
                         volume}).

-record(tt, {test}).
