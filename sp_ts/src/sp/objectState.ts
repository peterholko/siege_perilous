export interface ObjectState {
    id : string;
    player : string;
    name : string;
    class : string;
    subclass : string;
    template : string;
    state : string;
    hexX : integer;
    hexY: integer;
    prevHexX? : integer;
    prevHexY? : integer;
    vision : integer;
    image : string;
    op? : string;
    eventType? : string;
}