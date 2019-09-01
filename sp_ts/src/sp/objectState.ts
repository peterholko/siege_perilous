export interface ObjectState {
    id : string;
    player : string;
    name : string;
    class : string;
    subclass : string;
    template : string;
    state : string;
    prevstate : string;
    x : integer;
    y : integer;
    prevX? : integer;
    prevY? : integer;
    vision : integer;
    image : string;
    op? : string;
    eventType? : string;
}