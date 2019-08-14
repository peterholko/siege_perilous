import { ObjectState } from './objectState';
import { TileState } from './tileState';

export class Global {
    public static socket;
    public static game;
    public static gameEmitter; 
    public static uiEmitter;
    public static gameWidth = 666;
    public static gameHeight = 375;
    
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

    public static tileset = {};
    public static imageDefList = [];

    public static selectedItemId = -1;
    public static selectedItemOwnerId = -1;
}