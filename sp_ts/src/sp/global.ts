import { ObjectState } from './objectState';
import { TileState } from './tileState';
import { MultiImage } from './multiImage';

export class Global {
    public static socket;
    public static game;
    public static gameEmitter; 
    public static uiEmitter;
    public static gameWidth = 666;
    public static gameHeight = 375;

    public static serverOffline = false;
    public static networkError = false;

    public static heroDead = false;

    public static tick = 0;

    public static tileWidth = 72;
    public static tileHeight = 72;

    public static playerId = '-1';
    public static heroId = '-1';
    public static heroHp = 0;
    public static heroMaxHp = 0;
    public static heroSta = 0;
    public static heroMaxSta = 0;

    public static objectStates : Record<string, ObjectState> = {};
    public static tileStates : Record<string, TileState> = {};

    public static visibleTiles = [];

    public static tileset = {};
    public static imageDefList = {};

    public static selectedItemId = -1;
    public static selectedItemOwnerId = -1;
    public static selectedItemName = '';

    public static infoItemAction = 'inventory';
    public static infoItemTransferAction = 'transfer';
    
    public static merchantSellTarget;
}