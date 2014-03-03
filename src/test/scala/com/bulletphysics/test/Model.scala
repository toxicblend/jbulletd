package com.bulletphysics.test

import javax.vecmath.Vector3d
import com.bulletphysics.linearmath.AabbUtil2
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import com.bulletphysics.linearmath.AABB

class Model (val vertices:ArrayBuffer[Vector3d], val faces:ArrayBuffer[IndexedSeq[Int]]){
  /**
   * alternative constructor, note that it needs the origin to setup the AABB
   */
  def this(origin:Vector3d) = this({val a=new ArrayBuffer[Vector3d];a.append(origin);a}, new ArrayBuffer[IndexedSeq[Int]])
  
  val map = new HashMap[Vector3d,Int]()
  val bounds = {
    assert(vertices.size > 0)
    val aabb = new AABB(vertices(0))
    (0 until vertices.size).foreach( vi => {
      val v = vertices(vi)
      aabb.aabbExpand(v)
      map.put(v,vi)
    })
    aabb
  }
  def getVertices = vertices
  def getFaces = faces
  def getBounds = bounds
  def getOrAddVerticeIndex(v:Vector3d):Int = {
    if (map.contains(v)) {
      map(v)
    } else {
      val i=vertices.size
      vertices.append(v)
      map.put(v,i)
      i
    }
  }
  
  /**
   * Adds the face to the model (doesn't copy the vectors)
   */
  def addTriangle(v1:Vector3d, v2:Vector3d, v3:Vector3d) = {
    val newFace = Array(getOrAddVerticeIndex(v1), getOrAddVerticeIndex(v2),getOrAddVerticeIndex(v3))
    //println("Adding new face: " + newFace.mkString(",") )
    faces.append(newFace)
  }
  
  def addBox(center:Vector3d, extent:Vector3d) = {
    val extentHalf = new Vector3d(extent); extentHalf.scale(0.5)

    val v001 = new Vector3d(center.x - extentHalf.x, center.y - extentHalf.y, center.z + extentHalf.z)
    val v011 = new Vector3d(center.x - extentHalf.x, center.y + extentHalf.y, center.z + extentHalf.z)
    val v101 = new Vector3d(center.x + extentHalf.x, center.y - extentHalf.y, center.z + extentHalf.z)
    val v111 = new Vector3d(center.x + extentHalf.x, center.y + extentHalf.y, center.z + extentHalf.z)
    
    val v000 = new Vector3d(center.x - extentHalf.x, center.y - extentHalf.y, center.z - extentHalf.z) 
    val v010 = new Vector3d(center.x - extentHalf.x, center.y + extentHalf.y, center.z - extentHalf.z)
    val v100 = new Vector3d(center.x + extentHalf.x, center.y - extentHalf.y, center.z - extentHalf.z)
    val v110 = new Vector3d(center.x + extentHalf.x, center.y + extentHalf.y, center.z - extentHalf.z)
    
    // top side
    addTriangle(v001,v011,v101)
    addTriangle(v111,v011,v101)
    // bottom side
    addTriangle(v000,v010,v100)
    addTriangle(v110,v010,v100)
    // west side
    addTriangle(v000,v001,v011)
    addTriangle(v000,v010,v011)
    // east side
    addTriangle(v100,v101,v111)
    addTriangle(v100,v110,v111)
    // north side
    addTriangle(v011,v111,v010)
    addTriangle(v011,v110,v010)
    // south side
    addTriangle(v001,v101,v000)
    addTriangle(v001,v100,v000)
    
    //println(vertices.mkString(","))
  }
}