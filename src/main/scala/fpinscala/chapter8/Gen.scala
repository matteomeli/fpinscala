package fpinscala.chapter8

trait Prop {
  def check: Boolean

  // Exercise 8.3
  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
}