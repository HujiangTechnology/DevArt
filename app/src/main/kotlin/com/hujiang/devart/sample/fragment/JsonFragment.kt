package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.util.Log
import android.view.Menu
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.sample.jsontest.TestClass
import com.hujiang.devart.utils.FileUtils
import com.hujiang.devart.utils.JsonUtils

/**
 * Created by rarnu on 3/29/16.
 */
class JsonFragment : BaseFragment() {

    private var _tvObj2Json: TextView? = null
    private var _tvJson2Obj: TextView? = null
    private var _root: JsonUtils.JsonNode? = null
    private var _tc: TestClass? = null
    private var _ju: JsonUtils<TestClass>? = null

    override fun getBarTitle(): Int = R.string.json_name

    override fun getBarTitleWithPath(): Int = R.string.json_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _tvObj2Json = innerView?.findViewById(R.id.tvObj2Json) as TextView
        _tvJson2Obj = innerView?.findViewById(R.id.tvJson2Obj) as TextView
        _tc = TestClass()
        initNodeTree()
    }

    override fun initEvents() { }

    override fun initLogic() {

        // object 2 json
        val jstr = _ju?.toJson(_tc!!)
        _tvObj2Json?.text = jstr

        // json 2 object
        val jsonString = FileUtils.readAssetFile(activity, "json")
        val t = _ju?.toObject(jsonString!!)
        _tvJson2Obj?.text = t.toString()
    }


    /**
     * the tree structure is:
     * ```
     * |root
     * |----fa(V)
     * |----fb(V)
     * |----testInner(O)
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|----innerFA(V)
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|----innerFB(V)
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|----array(L)
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|----TestArray(O)
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|----arr(V)
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|----arrStr(L)
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|====String(V)
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|----map(M)
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|====String(V)
     * |----test(M)
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|----TestMap(O)
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|----map(V)
     * ```
     */
    private fun initNodeTree() {
        _root = JsonUtils.JsonNode("", JsonUtils.JsonNode.FieldType.ftObject)
        _root!!.childs!!.add(JsonUtils.JsonNode("fa", JsonUtils.JsonNode.FieldType.ftValue)) // fa
        _root!!.childs!!.add(JsonUtils.JsonNode("fb", JsonUtils.JsonNode.FieldType.ftValue)) // fb
        val inner = JsonUtils.JsonNode("testInner", JsonUtils.JsonNode.FieldType.ftObject) // testInner
        inner.childs!!.add(JsonUtils.JsonNode("innerFA", JsonUtils.JsonNode.FieldType.ftValue)); // testInner.innerFA
        inner.childs!!.add(JsonUtils.JsonNode("innerFB", JsonUtils.JsonNode.FieldType.ftValue)); // testInner.innerFB
        val innerArray = JsonUtils.JsonNode("array", JsonUtils.JsonNode.FieldType.ftList); // testInner.array
        innerArray.subItemNode = JsonUtils.JsonNode("TestArray", JsonUtils.JsonNode.FieldType.ftObject); // testInner.array<TestArray>
        innerArray.subItemNode!!.childs!!.add(JsonUtils.JsonNode("arr", JsonUtils.JsonNode.FieldType.ftValue)); // TestArray.arr
        inner.childs!!.add(innerArray)
        inner.childs!!.add(JsonUtils.JsonNode("arrStr", JsonUtils.JsonNode.FieldType.ftList, JsonUtils.JsonNode("arrStr", JsonUtils.JsonNode.FieldType.ftValue))); // inner.arrStr
        val innerMap = JsonUtils.JsonNode("map", JsonUtils.JsonNode.FieldType.ftMap); // inner.map
        innerMap.subItemNode = JsonUtils.JsonNode("String", JsonUtils.JsonNode.FieldType.ftValue); // inner.map<String>
        inner.childs!!.add(innerMap)
        _root!!.childs!!.add(inner)
        val test = JsonUtils.JsonNode("test", JsonUtils.JsonNode.FieldType.ftMap); // test
        test.subItemNode = JsonUtils.JsonNode("TestMap", JsonUtils.JsonNode.FieldType.ftObject); // test<TestMap>
        test.subItemNode!!.childs!!.add(JsonUtils.JsonNode("map", JsonUtils.JsonNode.FieldType.ftValue)); // TestMap.map
        _root!!.childs!!.add(test)
        _ju = JsonUtils(TestClass::class.java, _root!!)
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_json

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

}