import React from 'react';

export default function HeaderNav(props) {
	return (
		<section>
			<header>
				<nav className="rad-navigation">
					<div className="rad-logo-container">
						<a href="#" className="rad-logo">Payments Ledger</a>
						<a href="#" className="rad-toggle-btn pull-right"><i className="fa fa-bars"></i></a>
					</div>
					<a href="#" className="rad-logo-hidden">Payments Ledger</a>
					<div className="rad-top-nav-container">
						<form onSubmit={props.handleNuchainUrlSubmit}>
							Nuchain Node:&nbsp;
							<input size="60" type="text" value={props.nuchainUrl} onChange={props.handleNuchainUrlChange} />
							<input type="submit" value="Change" />
						</form>
					</div>
				</nav>
			</header>
		</section>
	);
}
