import React from 'react';
import { Col, Row, Container } from 'reactstrap';
import { Link } from 'react-router-dom';
import PropTypes from 'prop-types';

const background = `${process.env.PUBLIC_URL}/img/landing/header_bg_green.png`;
const img = `${process.env.PUBLIC_URL}/img/landing/macbook.png`;

const Header = ({ onClick }) => (
  <div className="landing__header" style={{ backgroundImage: `url(${background})` }}>
    <Container>
      <Row>
        <Col md={12}>
          <br></br>
          <h2 className="landing__header-title">Welcome to Sinuo, Amissa's artifical intelligence and analytics platform.</h2>
          {/* <p className="landing__header-subhead">We guarantee you will always get the actual version of the template
            with a bunch of{' '}
            <Link className="landing__header-subhead-update" to="/documentation/changelog" target="_blank">
              freshest updates
            </Link>.
          </p> */}
        </Col>
      </Row>
      <Row>
        <Col md={12}>
          <div className="landing__btn landing__btn--header"><a href="https://amissa.com/" target="_blank">
          {/* <Link className="landing__btn landing__btn--header" to="https://amissa.com/" target="_blank"> */}
            Visit Amissa.com
          {/* </Link> */}
          </a></div>
          {/* <button type="button" className="landing__btn landing__btn--header" onClick={onClick}> */}
          <button type="button" className="landing__btn landing__btn--header" to="/dashboard">
            Go to the Dashboard
          </button>
          <br></br>
          {/* <img className="landing__header-img" src={img} alt="macbook" /> */}
        </Col>
      </Row>
    </Container>
  </div>
);

Header.propTypes = {
  onClick: PropTypes.func.isRequired,
};

export default Header;
