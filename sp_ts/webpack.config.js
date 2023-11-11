var path = require('path');
var pathToPhaser = path.join(__dirname, '/node_modules/phaser/');
var phaser = path.join(pathToPhaser, 'dist/phaser.min.js');
const HtmlWebpackPlugin = require('html-webpack-plugin')


module.exports = {
  mode: "production",
  //devtool: "source-map",

  entry: './src/sp/main.tsx',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'sp2.js',
    //filename: "sp2.[chunkhash].js",
    //sourceMapFilename: '[name].[hash:8].map',
    //chunkFilename: '[id].[hash:8].js',
    library: 'SP'
  },
  module: {
    rules: [
      { 
        test: /\.ts(x?)$/, 
        use: "ts-loader",
        exclude: /node_modules/,
      },
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.(jpe?g|png|gif|woff|woff2|eot|ttf|svg)(\?[a-z0-9=.]+)?$/,
        use: {
          loader: 'url-loader',
          options: {
            limit: 150000
          }
        } 
      }
    ]
  },
  /*optimization: {
    splitChunks: {
      cacheGroups: {
        vendor: {
          test: /[\\/]node_modules[\\/]/,
          name: "phaser",
          enforce: true,
          chunks: "initial",
        },
      },
    },
  },*/
  resolve: {
    extensions: ['.ts', '.tsx', '.js'],
    alias: {
      phaser: phaser,
      ui: './../../../priv/static/art/ui/',
      ui_comp: './../../../../priv/static/art/ui/',
      art: './../../../priv/static/art/',
      art_comp: './../../../../priv/static/art/',
    }
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: path.resolve(__dirname, "./index.html"),
      filename: "index.html",
      title: "Siege Perilous",
      inject: "body",
      hot: true,
    }),
  ],
};
