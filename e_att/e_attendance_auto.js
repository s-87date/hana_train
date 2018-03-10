javascript:(
    function(){
        //現在、何週目のページにいるか確認
        var allHtml = document.getElementsByTagName('html')[0].innerHTML
        var indexOfWeekString = allHtml.search('週目')
        var indexOfWeek = allHtml.substring(indexOfWeekString - 1, indexOfWeekString)
        var indexOfMonthString = allHtml.search(escapeRegExp('<td headers="kinmutime" align="left" width="10%" nowrap="nowrap" style="border:1px solid #e6e6e6"'))
        var indexOfMonth = allHtml.substring(indexOfMonthString +103, indexOfMonthString + 105)

        function escapeRegExp(str) {
            return str.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
        }
        console.log(indexOfMonth + "月" + ("0" + indexOfWeek).slice(-2) + "週目の勤怠を入力します")

        /*
        通常勤務が選択されていたら、
        始業：09:00
        終業：18:00を入力
        休憩：01:00
        */
            for(var i = 1; i <= 31 ; i++) {
                var e = document.getElementById("wk_div" + i);
                if(e && e.options[e.selectedIndex].value == '11101') {
                    document.getElementById("wk_st_drn" + i).value = '09:00'
                    document.getElementById("wk_ed_drn" + i).value = '18:00'
                    document.getElementById("rst_drn" + i).value = '01:00'
                }
            }

        //次の週に遷移
        if(hasMorePage(indexOfMonth)){
            javascript:submitChangeStatus('EA_Next_Week', 1)
        }

        //月末まで入力していたらfalse(=画面遷移しない)
        function hasMorePage(indexOfMonth) {
            var monthEnd = new Date(new Date().getFullYear(), indexOfMonth, 0).getDate();
            console.log(monthEnd);
            return document.getElementById("wk_div" + monthEnd) == null ? true : false
        }
    })();

