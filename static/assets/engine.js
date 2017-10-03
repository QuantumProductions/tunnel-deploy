"use strict";

class Component {
  constructor(o) {
    this.components = {};
    this.listeners = {};
    this.installDefaults();
    this.canvasId = null;
    this.init(o);
  }

  draw(canvas, context) {
    if (canvas.id == this.canvasId) {
      this.paint(canvas, context);
    }
    this.msg('draw', {canvas: canvas, context: context});
  }

  paint(client, canvas) {
    //override
  }

  grab(name) {
    return this.components[name];
  }
  
  init(o) {}
  interestedTopics() {return [];}
  defaultInstalls() {return []; }

  installDefaults() {
    var defaultClasses = this.defaultInstalls();
    for (var i = 0; i < defaultClasses.length; i++) {
      var klass = defaultClasses[i];
      var comp = new klass();
      this.install(comp.constructor.name, comp);
    }

    for (let topic of this.interestedTopics()) {
      this.addListener(this, topic);
    }
  }

  install(name, c) {
    c.t = this;
    console.log("Installing" + name);
    this.components[name] = c;
    for (let topic of c.interestedTopics()) {
      console.log("Adding listener to component" + c.t);
      this.addListener(c, topic);
    }
  }

  addListener(c, topic) {
    if (!this.listeners[topic]) {
      this.listeners[topic] = [];
    }

    this.listeners[topic].push(c);
  }

  grab(name) {
    return this.components[name];
  }

  loop(dt) {
      
  }

  loopComponents() {
    var keys = Object.keys(this.components);
    for (let key of keys) {
      var c = this.components[key];
      this.components[key].loop();
    }
  }

  msg(title, body) {
      let listenerGroup = this.listeners[title];
      if (listenerGroup) {
        for (let listener of listenerGroup) {
          listener.handleMessage(title, body);
        } 
      }


      
    // let componentKeys = Object.keys(this.components);
    // for (let key of componentKeys) {
    //   var c = this.components[key];
    //   // console.log("key" + key);
    //   // console.log(this.components);
    //   // console.log(c);
    //   this.components[key].msg(title, body);
    // }
  }

  handleMessage(title, body) {

  }

  interestedTopics() {
    return [];
  }
}

class Thing extends Component {
  constructor(options) {
    super(options);
    this.components = {};
    if (options && options['position']) {
      this.x = options['position'][0];
      this.y = options['position'][1];
    } else {
      this.x = 2.0;
      this.y = 2.0; 
    }
    this.mx = 0;
    this.my = 0;
    this.active = true;

    this.init(options);
  }

  afterOutOfBounds() {

  }

  crossedLeft(x) {
    return this.x < x;
  }

  crossedTop(y) {
    return this.y < y;
  }

  crossedRight(x) {
    return this.x > x;
  }

  crossedBottom(y) {
    return this.y > y;
  }

  knows() {
    return [];
  }

  cares() {
    return false;
  }

  can() {
    return false;
  }

  does() {
    return {};
  }

  init(o) {
    if (!o) {
      return;
    }
  }

  speedMod() {
    return 1.0;
  }

  move(dt) {
    lateralMove(this);
  }

  loop(dt) {

  } 

  afterLoop() {
    this.loopComponents();
  }

  speed() {
    return 1.0;
  }

  position() {
    return [this.x, this.y];
  }
}
