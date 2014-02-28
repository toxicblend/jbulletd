package com.bulletphysics.test

import com.bulletphysics.util.ObjectArrayList
import com.bulletphysics.collision.broadphase.BroadphaseInterface
import com.bulletphysics.collision.broadphase.AxisSweep3_32
import com.bulletphysics.collision.broadphase.DbvtBroadphase
import com.bulletphysics.collision.shapes.BvhTriangleMeshShape
import com.bulletphysics.collision.shapes.CollisionShape
import com.bulletphysics.collision.shapes.TriangleIndexVertexArray
import com.bulletphysics.collision.shapes.ConeShapeZ
import com.bulletphysics.collision.shapes.ConvexShape
import com.bulletphysics.collision.shapes.UniformScalingShape
import com.bulletphysics.collision.dispatch.DefaultCollisionConfiguration
import com.bulletphysics.collision.dispatch.CollisionDispatcher
import com.bulletphysics.collision.dispatch.CollisionFlags
import com.bulletphysics.collision.dispatch.CollisionWorld
import com.bulletphysics.collision.dispatch.CollisionObject
import com.bulletphysics.collision.dispatch.CollisionWorld.ClosestRayResultCallback
import com.bulletphysics.linearmath.Transform
import com.bulletphysics.linearmath.DefaultMotionState
import com.bulletphysics.linearmath.VectorUtil
import scala.collection.mutable.ArrayBuffer
import java.nio.ByteBuffer
import java.nio.ByteOrder
import javax.vecmath.Vector3d

/**
 * A wrapper for the collision world, collision shapes and all the other references needed to simulate a collision.
 * This code is more or less just copy&paste from the toxicblend jbullet tests
 */
class CollisionWrapper(val models:IndexedSeq[Model]) {
  
  val collisionShapes = new ObjectArrayList[CollisionShape]();
  val convexShapes = new ArrayBuffer[ConvexShape]
 
  val vertStride = 3 * CollisionWrapper.VERTEX_S
  val indexStride = 3 * CollisionWrapper.INDEX_S
  val totalVerts = models(0).getVertices.size
  val totalTriangles = models(0).getFaces.size
   
  val gVertices = ByteBuffer.allocateDirect(totalVerts * 3 * CollisionWrapper.VERTEX_S).order(ByteOrder.nativeOrder());
  val gIndices = ByteBuffer.allocateDirect(totalTriangles * 3 * CollisionWrapper.INDEX_S).order(ByteOrder.nativeOrder());
  
  val aabbAllModels = {
    if (models.size > 0){
      val aabbTmp = models(0).getBounds.copy
      models.tail.foreach(b => aabbTmp.union(b.getBounds))
      aabbTmp
    } else {
      new AABB
    }
  }
  val zMin = aabbAllModels.getMin.z-1f
  val zMax = aabbAllModels.getMax.z+1f
  
  (0 until totalVerts).foreach(index => {
    val v = models(0).getVertices(index)
    gVertices.putDouble((index*3 + 0) * CollisionWrapper.VERTEX_S, v.x)
    gVertices.putDouble((index*3 + 1) * CollisionWrapper.VERTEX_S, v.y)
    gVertices.putDouble((index*3 + 2) * CollisionWrapper.VERTEX_S, v.z)
  });
  
  {
    //println("totalVerts:" + totalVerts)
    //println("totalTriangles:" + totalTriangles)
    val faces = models(0).getFaces
    (0 until totalTriangles).foreach(index => {
      val face = faces(index)
      if (face.size > 3 ) throw new BulletException("JBullet mesh must be triangulated")
      else if (face.size == 3) {
        gIndices.putInt((index*3 + 0) * CollisionWrapper.INDEX_S, face(0))
        gIndices.putInt((index*3 + 1) * CollisionWrapper.INDEX_S, face(1))
        gIndices.putInt((index*3 + 2) * CollisionWrapper.INDEX_S, face(2))
      }
      // silently ignore edges and unconnected vertices
    })
  }
  
  val indexVertexArrays = new TriangleIndexVertexArray(totalTriangles,gIndices,indexStride,totalVerts,gVertices,vertStride)
  
  val useQuantizedAabbCompression = true
  
  val trimeshShape = new BvhTriangleMeshShape(indexVertexArrays, useQuantizedAabbCompression)
  collisionShapes.add(trimeshShape)

  val groundShape:CollisionShape = trimeshShape
  val collisionConfiguration = new DefaultCollisionConfiguration()
  val dispatcher = new CollisionDispatcher(collisionConfiguration)
  val broadphase:BroadphaseInterface = if (true) {
      val worldMin = models(0).getBounds.getMin
      val worldMax = models(0).getBounds.getMax
      //println("worldMin=" + worldMin)
      //println("worldMax=" + worldMax)
      new AxisSweep3_32(worldMin, worldMax, 1500000/2);
    } else {
    new DbvtBroadphase
  }
  
  //val broadphase:BroadphaseInterface = new DbvtBroadphase()
  val collisionWorld = new CollisionWorld(dispatcher, broadphase, collisionConfiguration)
  val startTransform = new Transform
  startTransform.setIdentity();
  startTransform.origin.set(0, 0, 0);

  val staticBody = localCreateCollisionObject(startTransform, groundShape)

  staticBody.setCollisionFlags(staticBody.getCollisionFlags() | CollisionFlags.STATIC_OBJECT);

  // enable custom material callback
  //staticBody.setCollisionFlags(staticBody.getCollisionFlags() | CollisionFlags.CUSTOM_MATERIAL_CALLBACK);
  
  def localCreateCollisionObject(startTransform:Transform, shape:CollisionShape):CollisionObject = {   
    val myMotionState = new DefaultMotionState(startTransform)
    val collisionobject = new CollisionObject
    collisionobject.setCollisionShape(shape)
    collisionobject.setWorldTransform(startTransform)
    collisionWorld.addCollisionObject(collisionobject)
    collisionobject
  }
  
  def addVCutter(radius:Double, height:Double):ConvexShape = {
    val margin = 0.02;
     val colShape:ConvexShape = new ConeShapeZ(2d, 2d);
     colShape.setMargin(margin)
     collisionShapes.add(colShape);
     val convexShape = new UniformScalingShape(colShape, 1d)
     convexShapes.append(convexShape)
     convexShape
  }
  
  def destroy = {
    collisionWorld.destroy
  }
  
  def rayTest(rayFromWorld:Vector3d,rayToWorld:Vector3d, result:Vector3d) = {
    val cb = new ClosestRayResultCallback(rayFromWorld,rayToWorld)
    collisionWorld.rayTest(rayFromWorld,rayToWorld,cb)
    val hitPointWorld = new Vector3d
    //println("rayFromWorld=" + rayFromWorld)
    //println("rayToWorld=" + rayToWorld)
    //println("cb.closestHitFraction=" + cb.closestHitFraction)
    if (cb.closestHitFraction >= 1.0) {
      // no hit
      result.x=rayToWorld.x
      result.y=rayToWorld.y
      result.z=rayToWorld.z
      false
    } else {
      VectorUtil.setInterpolate3(result, rayFromWorld, rayToWorld, cb.closestHitFraction)
      true
    }
  }
}

object CollisionWrapper {
  val VERTEX_S = 8 // a double
  val INDEX_S = 4  // an int
  
  /**
   * Alternative constructor
   
  def apply(__segment:IndexedSeq[Vector3d], __models:IndexedSeq[Model]) = {
    new CollisionWrapper(Array(__segment), __models)
  }
  */
}