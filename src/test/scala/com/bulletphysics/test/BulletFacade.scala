package com.bulletphysics.test

import com.bulletphysics.util.ObjectArrayList
import com.bulletphysics.collision.broadphase.BroadphaseInterface
import com.bulletphysics.collision.broadphase.AxisSweep3_32
import com.bulletphysics.collision.broadphase.DbvtBroadphase
import com.bulletphysics.collision.shapes.BvhTriangleMeshShape
import com.bulletphysics.collision.shapes.CollisionShape
import com.bulletphysics.collision.shapes.TriangleIndexVertexArray
import com.bulletphysics.collision.shapes.ConeShapeZ
import com.bulletphysics.collision.shapes.SphereShape
import com.bulletphysics.collision.shapes.ConvexShape
import com.bulletphysics.collision.shapes.UniformScalingShape
import com.bulletphysics.collision.dispatch.DefaultCollisionConfiguration
import com.bulletphysics.collision.dispatch.CollisionDispatcher
import com.bulletphysics.collision.dispatch.CollisionFlags
import com.bulletphysics.collision.dispatch.CollisionWorld
import com.bulletphysics.collision.dispatch.CollisionObject
import com.bulletphysics.collision.dispatch.CollisionWorld.ClosestRayResultCallback
import com.bulletphysics.collision.dispatch.CollisionWorld.ClosestConvexResultCallback
import com.bulletphysics.linearmath.Transform
import com.bulletphysics.linearmath.DefaultMotionState
import com.bulletphysics.linearmath.VectorUtil
import scala.collection.mutable.ArrayBuffer
import java.nio.ByteBuffer
import java.nio.ByteOrder
import javax.vecmath.Vector3d
import javax.vecmath.Matrix3d
import javax.vecmath.AxisAngle4d
import com.bulletphysics.linearmath.AABB

/**
 * A wrapper for the collision world, collision shapes and all the other references needed to simulate a collision.
 * This code is more or less just copy&paste from the toxicblend jbullet tests
 */
class BulletFacade(val models:IndexedSeq[Model], val zMinMod:Double) {
  
  val collisionShapes = new ObjectArrayList[CollisionShape]();
  val convexShapes = new ArrayBuffer[ConvexShape]
 
  val vertStride = 3 * BulletFacade.VERTEX_S
  val indexStride = 3 * BulletFacade.INDEX_S
  val totalVerts = models(0).getVertices.size
  val totalTriangles = models(0).getFaces.size
   
  val gVertices = ByteBuffer.allocateDirect(totalVerts * 3 * BulletFacade.VERTEX_S).order(ByteOrder.nativeOrder());
  val gIndices = ByteBuffer.allocateDirect(totalTriangles * 3 * BulletFacade.INDEX_S).order(ByteOrder.nativeOrder());
  
  val aabbAllModels = new AABB(models.map(m=>m.bounds))
   
  val zMin = aabbAllModels.getMin.z-zMinMod
  val zMax = aabbAllModels.getMax.z+1d
  
  (0 until totalVerts).foreach(index => {
    val v = models(0).getVertices(index)
    gVertices.putDouble((index*3 + 0) * BulletFacade.VERTEX_S, v.x)
    gVertices.putDouble((index*3 + 1) * BulletFacade.VERTEX_S, v.y)
    gVertices.putDouble((index*3 + 2) * BulletFacade.VERTEX_S, v.z)
  });
  
  {
    //println("totalVerts:" + totalVerts)
    //println("totalTriangles:" + totalTriangles)
    val faces = models(0).getFaces
    (0 until totalTriangles).foreach(index => {
      val face = faces(index)
      if (face.size > 3 ) throw new BulletException("JBullet mesh must be triangulated")
      else if (face.size == 3) {
        gIndices.putInt((index*3 + 0) * BulletFacade.INDEX_S, face(0))
        gIndices.putInt((index*3 + 1) * BulletFacade.INDEX_S, face(1))
        gIndices.putInt((index*3 + 2) * BulletFacade.INDEX_S, face(2))
      }
      // silently ignore edges and unconnected vertices
    })
  }
  
  val indexVertexArrays = new TriangleIndexVertexArray(totalTriangles, gIndices, indexStride, totalVerts, gVertices, vertStride)
  
  val useQuantizedAabbCompression = true
  
  val trimeshShape = new BvhTriangleMeshShape(indexVertexArrays, useQuantizedAabbCompression)
  collisionShapes.add(trimeshShape)

  val groundShape:CollisionShape = trimeshShape
  val collisionConfiguration = new DefaultCollisionConfiguration()
  val dispatcher = new CollisionDispatcher(collisionConfiguration)
  val broadphase:BroadphaseInterface = if (true) {
      new AxisSweep3_32(aabbAllModels.getMin, aabbAllModels.getMax, 1500000/2);;
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
  
  def addConeShapeZ(radius:Double, height:Double, margin:Double):ConvexShape = {
    val colShape:ConvexShape = new ConeShapeZ(radius, height);
    colShape.setMargin(margin)
    collisionShapes.add(colShape);
    colShape
    //val convexShape = new UniformScalingShape(colShape, 1f)
    //convexShapes.append(convexShape)
    //convexShape
  }
  
   def addShereShape(radius:Double):ConvexShape = {
    val colShape:ConvexShape = new SphereShape(radius);
    collisionShapes.add(colShape);
    colShape
    //val convexShape = new UniformScalingShape(colShape, 1f)
    //convexShapes.append(convexShape)
    //convexShape
  }
  
  def destroy = collisionWorld.destroy
  
  /**
   * perform an inefficient rayTest
   */
  def rayTest(rayFromWorld:Vector3d, rayToWorld:Vector3d, result:Vector3d) = {
    val cb = new ClosestRayResultCallback(rayFromWorld, rayToWorld)
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
  
  /**
   * perform an inefficient convexSweepTest
   */
  def convexSweepTest(convexShape:ConvexShape, rayFromWorld:Vector3d, rayToWorld:Vector3d, rot:AxisAngle4d, result:Vector3d) = {
    val fromTransform = new Transform()
    fromTransform.basis.set(rot)
    fromTransform.origin.set(rayFromWorld)
    
    val toTransform = new Transform()
    toTransform.basis.set(rot)
    toTransform.origin.set(rayToWorld)

    val convexCallback = new ClosestConvexResultCallback(new Vector3d(rayFromWorld), new Vector3d(rayToWorld))
    collisionWorld.convexSweepTest(convexShape, fromTransform, toTransform, convexCallback);
    if (!convexCallback.hasHit()) {
      // no hit
      result.x=rayToWorld.x
      result.y=rayToWorld.y
      result.z=rayToWorld.z-10
      false
    } else {
      VectorUtil.setInterpolate3(result, rayFromWorld, rayToWorld, convexCallback.closestHitFraction)
      //result.x=convexCallback.hitPointWorld.x 
      //result.y=convexCallback.hitPointWorld.y
      //result.z=convexCallback.hitPointWorld.z
      // VectorUtil.setInterpolate3(result, rayFromWorld, rayToWorld, convexCallback.closestHitFraction)
      true
    }
  }
}

object BulletFacade {
  val VERTEX_S = 8 // a double
  val INDEX_S = 4  // an int
  
  /**
   * Alternative constructor
   
  def apply(__segment:IndexedSeq[Vector3d], __models:IndexedSeq[Model]) = {
    new CollisionWrapper(Array(__segment), __models)
  }
  */
}