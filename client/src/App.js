import React, { Component } from 'react';
import { render } from 'react-dom'
import { Router, Route, IndexRoute, Link, hashHistory } from 'react-router';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import AppBar from 'material-ui/AppBar';
import IconButton from 'material-ui/IconButton';
import IconMenu from 'material-ui/IconMenu';
import MenuItem from 'material-ui/MenuItem';
import Drawer from 'material-ui/Drawer';
import MoreVertIcon from 'material-ui/svg-icons/navigation/more-vert';
import Home from './scenes/home/home';
import About from './scenes/about/about';
import Register from './scenes/register/register';
import SignIn from './scenes/signIn/signIn';
import './App.css';

import injectTapEventPlugin from 'react-tap-event-plugin';
injectTapEventPlugin();

const MoreMenu = (props) => (
	<IconMenu
		{...props}
		iconButtonElement={
			<IconButton><MoreVertIcon /></IconButton>
		}
		targetOrigin={{horizontal: 'right', vertical: 'top'}}
		anchorOrigin={{horizontal: 'right', vertical: 'top'}}
	>
		<MenuItem
			primaryText="Help"
			containerElement={<Link to="/help" />}
		/>
		<MenuItem
			primaryText="Register"
			containerElement={<Link to="/register" />}
		/>
		<MenuItem
			primaryText="Sign In"
			containerElement={<Link to="/signIn" />}
		/>
	</IconMenu>
);

MoreMenu.muiName = 'IconMenu';

class App extends Component {
	constructor(props) {
		super(props);

		this.state = {
			open: false
		};
	}

	toggleDrawer() {
		this.setState({
			open: !this.state.open
		});
	}

	render() {
		return (
			<MuiThemeProvider>
				<div>
					<Drawer open={this.state.open}>
						<MenuItem
							primaryText="Home"
							containerElement={<Link to="/home" />}
						/>
						<MenuItem
							primaryText="About"
							containerElement={<Link to="/about" />}
						/>
					</Drawer>
					<AppBar
						title="Informatio"
						onLeftIconButtonTouchTap={this.toggleDrawer.bind(this)}
						iconElementRight={<MoreMenu />}
					/>
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
			<Route path="/home" component={Home} />
			<Route path="/about" component={About} />
			<Route path="/register" component={Register} />
			<Route path="/signIn" component={SignIn} />
		</Route>
	</Router>
), document.body);

export default App;