package com.hujiang.devart.sample.fragment

import android.app.AlertDialog
import android.graphics.*
import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.AdapterView
import android.widget.Button
import android.widget.ImageView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.draggrid.DraggableGridView
import com.hujiang.devart.component.draggrid.OnRearrangeListener
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import java.util.*

/**
 * Created by rarnu on 4/14/16.
 */
class DragGridFragment: BaseFragment(), OnRearrangeListener, AdapterView.OnItemClickListener, View.OnClickListener {

    companion object {
        val random = Random()
        val words = "the of and a to in is be that was he for it with as his I on have at by not they this had are but from or she an which you one we all were her would there their will when who him been has more if no out do so can what up said about other into than its time only could new them man some these then two first may any like now my such make over our even most me state after also made many did must before back see through way where get much go well your know should down work year because come people just say each those take day good how long Mr own too little use US very great still men here life both between old under last never place same another think house while high right might came off find states since used give against three himself look few general hand school part small American home during number again Mrs around thought went without however govern don't does got public United point end become head once course fact upon need system set every war put form water took".split(" ")
    }

    private var _dgv: DraggableGridView? = null
    private var _btnAddWord: Button? = null
    private var _btnViewPoem: Button? = null
    private var _poem = arrayListOf<String>()

    override fun getBarTitle(): Int = R.string.draggrid_name

    override fun getBarTitleWithPath(): Int = R.string.draggrid_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _dgv = innerView?.findViewById(R.id.dgv) as DraggableGridView
        _btnAddWord = innerView?.findViewById(R.id.btnAddWord) as Button
        _btnViewPoem = innerView?.findViewById(R.id.btnViewPoem) as Button
    }

    override fun initEvents() {
        _dgv?.setOnRearrangeListener(this)
        _dgv?.setOnItemClickListener(this)
        _btnAddWord?.setOnClickListener(this)
        _btnViewPoem?.setOnClickListener(this)
    }

    override fun initLogic() {

    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_drag_grid

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null


    override fun onRearrange(oldIndex: Int, newIndex: Int) {
        val word = _poem.removeAt(oldIndex)
        if (oldIndex < newIndex) {
            _poem.add(newIndex, word)
        } else {
            _poem.add(newIndex, word)
        }
    }

    override fun onItemClick(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        _dgv?.removeViewAt(position)
        _poem.removeAt(position)
    }

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.btnAddWord -> {
                val word = words[random.nextInt(words.size)]
                val view = ImageView(activity)
                view.setImageBitmap(getThumb(word))
                _dgv?.addView(view)
                _poem.add(word)
            }
            R.id.btnViewPoem -> {
                var finishedPoem = ""
                for (s in _poem)
                finishedPoem += s + " "
                AlertDialog.Builder(activity).setTitle("Here's your poem!").setMessage(finishedPoem).show()
            }
        }
    }

    private fun getThumb(s: String?): Bitmap? {
        val bmp = Bitmap.createBitmap(150, 150, Bitmap.Config.RGB_565)
        val canvas = Canvas(bmp)
        val paint = Paint()
        paint.color = Color.rgb(random.nextInt(128), random.nextInt(128), random.nextInt(128))
        paint.textSize = 24.toFloat()
        paint.flags = Paint.ANTI_ALIAS_FLAG
        canvas.drawRect(Rect(0, 0, 150, 150), paint)
        paint.color = Color.WHITE
        paint.textAlign = Paint.Align.CENTER
        canvas.drawText(s, 75.0f, 75.0f, paint)
        return bmp
    }
}