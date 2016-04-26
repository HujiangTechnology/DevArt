package com.hujiang.devart.component.cropper

/**
 * Created by rarnu on 4/26/16.
 */
class EdgePair {

    var primary: Edge?
    var secondary: Edge?

    constructor(edge1: Edge?, edge2: Edge?) {
        primary = edge1
        secondary = edge2
    }

}