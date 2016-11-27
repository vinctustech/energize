import React, { Component } from 'react';
import { render } from 'react-dom'
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import { Router, Route, IndexRoute, Link, hashHistory } from 'react-router';
import Home from './scenes/home';
import About from './scenes/about';
import logo from './logo.svg';
import './App.css';

class App extends Component {
	render() {
		return (
			<MuiThemeProvider>
				<div>
					<h1>Informatio</h1>
					<ul>
						<li><Link to="/home">Home</Link></li>
						<li><Link to="/about">About</Link></li>
					</ul>
					{this.props.children}
				</div>

			</MuiThemeProvider>
		);
	}
}

render((
  <Router history={hashHistory}>
    <Route path="/" component={App}>
      <IndexRoute component={Home} />
      <Route path="home" component={Home} />
      <Route path="about" component={About} />
    </Route>
  </Router>
), document.body);

export default App;