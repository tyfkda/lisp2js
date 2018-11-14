import path from 'path'
import webpack from 'webpack'

const webpackJsLispConfig = {
  context: __dirname + '/src/runtime',
  mode: "production",
  devtool: "cheap-source-map",
  target: "node",
  entry: {
    jslisp: './jslisp.js',
  },
  output: {
    path: path.resolve(__dirname, 'bin'),
    filename: '[name].js',
    sourceMapFilename: '[name].map',
  },
  resolve: {
    extensions: ['.js'],
  },
  module: {
    rules: [
      { test: /\.js$/, exclude: /node_modules/, use: { loader: 'babel-loader' } },
    ],
  },
}

const webpackLisp2JsConfig = {
  context: __dirname + '/src/runtime',
  mode: "production",
  devtool: "cheap-source-map",
  target: "web",
  entry: {
    lisp2js: './lisp2js.js',
  },
  output: {
    path: path.resolve(__dirname, 'bin'),
    filename: '[name].js',
    sourceMapFilename: '[name].map',
  },
  resolve: {
    extensions: ['.js'],
  },
  module: {
    rules: [
      { test: /\.js$/, exclude: /node_modules/, use: { loader: 'babel-loader' } },
    ],
  },
}

module.exports = {
  webpackJsLispConfig,
  webpackLisp2JsConfig,
}
