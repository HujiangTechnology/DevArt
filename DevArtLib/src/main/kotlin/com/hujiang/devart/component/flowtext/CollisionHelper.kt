package com.hujiang.devart.component.flowtext

/**
 * Created by rarnu on 4/15/16.
 */
object CollisionHelper {

    private val _areas = arrayListOf<Area>()

    fun calculateLineSpaceForGivenYOffset(lineYbottom: Float, lineHeight: Int, viewWidth: Float, obstacles: MutableList<Obstacle>): Line {
        val line = Line()
        line.leftBound = 0.0f
        line.rightBound = viewWidth
        val lineYtop = lineYbottom - lineHeight
        _areas.clear()
        for (obstacle in obstacles) {
            if(obstacle.topLefty > lineYbottom || obstacle.bottomRighty < lineYtop){
            }else{
                val leftArea = Area()
                leftArea.x1 = 0.0f
                for (innerObstacle in obstacles) {
                    if(innerObstacle.topLefty > lineYbottom || innerObstacle.bottomRighty < lineYtop){
                    }else{
                        if(innerObstacle.topLeftx < obstacle.topLeftx){
                            leftArea.x1 = innerObstacle.bottomRightx.toFloat()
                        }
                    }
                }
                leftArea.x2 = obstacle.topLeftx.toFloat()
                leftArea.width = leftArea.x2 - leftArea.x1
                val rightArea = Area()
                rightArea.x1 = obstacle.bottomRightx.toFloat()
                rightArea.x2 = viewWidth
                for (innerObstacle in obstacles) {
                    if(innerObstacle.topLefty > lineYbottom || innerObstacle.bottomRighty < lineYtop){
                    }else{
                        if(innerObstacle.bottomRightx > obstacle.bottomRightx){
                            rightArea.x2 = innerObstacle.topLeftx.toFloat()
                        }
                    }
                }
                rightArea.width = rightArea.x2 - rightArea.x1
                _areas.add(leftArea)
                _areas.add(rightArea)
            }
        }
        var largestArea: Area? = null
        if(_areas.size >0) {
            for (area in _areas) {
                if(largestArea == null){
                    largestArea = area
                }else{
                    if(area.width > largestArea.width){
                        largestArea = area
                    }
                }
            }
            line.leftBound = largestArea!!.x1
            line.rightBound = largestArea.x2
        }
        return line
    }


}