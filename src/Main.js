// module Main

import React from 'react';
import ReactDOM from 'react-dom';

const Block = React.createClass({
  render() {
    const {increment} = this.props;

    return (
      <rect
        key={this.props.key}
        x={increment * this.props.coords.x}
        y={increment * this.props.coords.y}
        width={increment}
        height={increment}
      />
    );
  }
});

const Board = React.createClass({
  drawPath() {
    const state: State = this.props.state;
    const {points} = state;
    const elements = [];

    for (let key in points) {
      if (points.hasOwnProperty(key)) {
        elements.push(
          <Block
            key={key}
            increment={this.props.increment}
            coords={points[key]}
          />
        );
      }
    }

    return elements;
  },

  drawCursor(){
    return (
      <Block
        key="cursor"
        increment={this.props.increment}
        coords={this.props.state.cursor}
      />
    );
  },

  render() {
    return (
      <svg width={this.props.width} height={this.props.height}>
        {this.drawPath()}
        {this.drawCursor()}
      </svg>
    );
  }
});

const App = React.createClass({
  contextTypes: {
    boardClear$: React.PropTypes.any
  },

  render(): ReactElement {
    const state = this.props.state;

    return (
      <div>
        <div style={{border: '1px solid black', width: state.width, height: state.height}}>
          <Board
            state={state}
            width={state.width}
            height={state.height}
            increment={state.increment}
          />
        </div>
      </div>
    )
  }
});

export function jsRender(props) {
  return function () {
    console.log('props', props);
    ReactDOM.render(
      <App state={props}/>,
      document.getElementById('app')
    );
  }
}

export function jsRenderError() {
  return function () {
    console.error('we blew up');
  }
}
