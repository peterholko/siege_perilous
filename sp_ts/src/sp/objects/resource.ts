export class Resource extends Phaser.GameObjects.Image {

    public hexX;
    public hexY;

    constructor(params) {
        super(params.scene, params.x, params.y, params.imageName);

        this.hexX = params.hexX;
        this.hexY = params.hexY;

        this.initImage();
    }

    private initImage(): void {
        this.setOrigin(0.0);
      }

}