package com.bulletphysics.test

import javax.vecmath.Vector3d
import scala.collection.mutable.MutableList
import org.scalatest._

class RaycastTest1 extends FlatSpec with Matchers {
  
  val CLOSETO1 = 0.9999
  
  val model = new Model(new Vector3d)
  model.addBox(new Vector3d, new Vector3d(2,2,2))
  //model.addTriangle(new Vector3d(0,0,0), new Vector3d(0,1,0), new Vector3d(1,0,0))
  //model.addTriangle(new Vector3d(1,1,1), new Vector3d(1,2,1), new Vector3d(2,1,1))
  
  val models = Array(model)
  val jbcw = new CollisionWrapper(models,5)
  
  "ray test1" should "collide just fine" in {
    val fromV = new Vector3d(0.1,0.1,10)
    val toV = new Vector3d(0.1,0.1,-10)
    val result = new Vector3d()
    var rv = jbcw.rayTest(fromV,toV,result)
    rv should be (true)
    result.x should be (toV.x)
    result.y should be (toV.y)
    result.z should be (1)
  }
  
  "ray test2" should "collide just fine" in {
    val fromV = new Vector3d(CLOSETO1,CLOSETO1,10)
    val toV = new Vector3d(CLOSETO1,CLOSETO1,-10)
    val result = new Vector3d()
    var rv = jbcw.rayTest(fromV,toV,result)
    rv should be (true)
    result.x should be (toV.x)
    result.y should be (toV.y)
    result.z should be (1)
  }
  
  "ray test3" should "collide just fine" in {
    val fromV = new Vector3d(-CLOSETO1,CLOSETO1,10)
    val toV = new Vector3d(-CLOSETO1,CLOSETO1,-10)
    val result = new Vector3d()
    var rv = jbcw.rayTest(fromV,toV,result)
    rv should be (true)
    result.x should be (toV.x)
    result.y should be (toV.y)
    result.z should be (1)
  }
  
  "ray test4" should "collide just fine" in {
    val fromV = new Vector3d(0,-CLOSETO1,10)
    val toV = new Vector3d(0,-CLOSETO1,-10)
    val result = new Vector3d()
    var rv = jbcw.rayTest(fromV,toV,result)
    rv should be (true)
    result.x should be (toV.x)
    result.y should be (toV.y)
    result.z should be (1)
  }
  
  "ray test5" should "collide just fine" in {
    val fromV = new Vector3d(-CLOSETO1,-CLOSETO1,10)
    val toV = new Vector3d(-CLOSETO1,-CLOSETO1,-10)
    val result = new Vector3d()
    var rv = jbcw.rayTest(fromV,toV,result)
    rv should be (true)
    result.x should be (toV.x)
    result.y should be (toV.y)
    result.z should be (1)
  }
  
  "ray test6" should "collide just fine" in {
    val fromV = new Vector3d(-CLOSETO1,-CLOSETO1,-10)
    val toV = new Vector3d(-CLOSETO1,-CLOSETO1,10)
    val result = new Vector3d()
    var rv = jbcw.rayTest(fromV,toV,result)
    rv should be (true)
    result.x should be (toV.x)
    result.y should be (toV.y)
    result.z should be (-1)
  }
  
  "ray test7" should "collide just fine" in {
    val fromV = new Vector3d(CLOSETO1,-CLOSETO1,-10)
    val toV = new Vector3d(CLOSETO1,-CLOSETO1,10)
    val result = new Vector3d()
    var rv = jbcw.rayTest(fromV,toV,result)
    rv should be (true)
    result.x should be (toV.x)
    result.y should be (toV.y)
    result.z should be (-1)
  }
  
  "ray test8" should "collide just fine" in {
    val fromV = new Vector3d(CLOSETO1,CLOSETO1,-10)
    val toV = new Vector3d(CLOSETO1,CLOSETO1,10)
    val result = new Vector3d()
    var rv = jbcw.rayTest(fromV,toV,result)
    rv should be (true)
    result.x should be (toV.x)
    result.y should be (toV.y)
    result.z should be (-1)
  }
  
  "ray test9" should "collide just fine" in {
    val fromV = new Vector3d(-10,-CLOSETO1,CLOSETO1)
    val toV = new Vector3d(10,-CLOSETO1,CLOSETO1)
    val result = new Vector3d()
    var rv = jbcw.rayTest(fromV,toV,result)
    rv should be (true)
    result.z should be (toV.z)
    result.y should be (toV.y)
    result.x should be (-1)
  }
  
  "ray test10" should "collide just fine" in {
    val fromV = new Vector3d(10,-CLOSETO1,CLOSETO1)
    val toV = new Vector3d(-10,-CLOSETO1,CLOSETO1)
    val result = new Vector3d()
    var rv = jbcw.rayTest(fromV,toV,result)
    rv should be (true)
    result.z should be (toV.z)
    result.y should be (toV.y)
    result.x should be (1)
  }
  "ray test cleanup" should "not fail" in {
    jbcw.destroy
  }
}