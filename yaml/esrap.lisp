
  
  

  


<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <meta http-equiv="content-type" content="text/html;charset=UTF-8" />
    <meta http-equiv="X-UA-Compatible" content="chrome=1">
        <title>esrap.lisp at master from nikodemus's esrap - GitHub</title>
    <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub" />
    <link rel="fluid-icon" href="http://github.com/fluidicon.png" title="GitHub" />

    <link href="http://assets0.github.com/stylesheets/bundle_common.css?7371c81fbc6b010a32fb11b42a0fc322c3c57863" media="screen" rel="stylesheet" type="text/css" />
<link href="http://assets0.github.com/stylesheets/bundle_github.css?7371c81fbc6b010a32fb11b42a0fc322c3c57863" media="screen" rel="stylesheet" type="text/css" />

    <script type="text/javascript" charset="utf-8">
      var GitHub = {}
      var github_user = null
    </script>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.1/jquery.min.js" type="text/javascript"></script>
    <script src="http://assets2.github.com/javascripts/bundle_common.js?7371c81fbc6b010a32fb11b42a0fc322c3c57863" type="text/javascript"></script>
<script src="http://assets3.github.com/javascripts/bundle_github.js?7371c81fbc6b010a32fb11b42a0fc322c3c57863" type="text/javascript"></script>

        <script type="text/javascript" charset="utf-8">
      GitHub.spy({
        repo: "nikodemus/esrap"
      })
    </script>

    
  
    
  

  <link href="http://github.com/feeds/nikodemus/commits/esrap/master" rel="alternate" title="Recent Commits to esrap:master" type="application/atom+xml" />

    <meta name="description" content="Common Lisp packrat parser" />
    <script type="text/javascript">
      GitHub.nameWithOwner = GitHub.nameWithOwner || "nikodemus/esrap";
      GitHub.currentRef = "master";
    </script>
  

            <script type="text/javascript">
      var _gaq = _gaq || [];
      _gaq.push(['_setAccount', 'UA-3769691-2']);
      _gaq.push(['_trackPageview']);
      (function() {
        var ga = document.createElement('script');
        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
        ga.setAttribute('async', 'true');
        document.documentElement.firstChild.appendChild(ga);
      })();
    </script>

  </head>

  

  <body>
    

    

    <div class="subnavd" id="main">
      <div id="header" class="pageheaded">
        <div class="site">
          <div class="logo">
            <a href="http://github.com"><img src="/images/modules/header/logov3.png" alt="github" /></a>
          </div>
          
          <div class="topsearch">
  
    <form action="/search" id="top_search_form" method="get">
      <a href="/search" class="advanced-search tooltipped downwards" title="Advanced Search">Advanced Search</a>
      <input type="search" class="search repo_autocompleter" name="q" results="5" placeholder="Search&hellip;" /> <input type="submit" value="Search", class="button" />
      <input type="hidden" name="type" value="Everything" />
      <input type="hidden" name="repo" value="" />
      <input type="hidden" name="langOverride" value="" />
      <input type="hidden" name="start_value" value="1" />
    </form>
  
  
    <ul class="nav logged_out">
      
        <li><a href="http://github.com">Home</a></li>
        <li class="pricing"><a href="/plans">Pricing and Signup</a></li>
        <li><a href="http://github.com/explore">Explore GitHub</a></li>
        
        <li><a href="/blog">Blog</a></li>
      
      <li><a href="https://github.com/login">Login</a></li>
    </ul>
  
</div>

        </div>
      </div>

      
      
        
    <div class="site">
      <div class="pagehead repohead vis-public  ">
        <h1>
          <a href="/nikodemus">nikodemus</a> / <strong><a href="http://github.com/nikodemus/esrap">esrap</a></strong>
          
          
        </h1>

        
    <ul class="actions">
      
      
        <li class="for-owner" style="display:none"><a href="https://github.com/nikodemus/esrap/edit" class="minibutton btn-admin "><span><span class="icon"></span>Admin</span></a></li>
        <li>
          <a href="/nikodemus/esrap/toggle_watch" class="minibutton btn-watch " id="watch_button" style="display:none"><span><span class="icon"></span>Watch</span></a>
          <a href="/nikodemus/esrap/toggle_watch" btn_class="watch" class="minibutton btn-watch " id="unwatch_button" style="display:none"><span><span class="icon"></span>Unwatch</span></a>
        </li>
        
          <li class="for-notforked" style="display:none"><a href="/nikodemus/esrap/fork" class="minibutton btn-fork " id="fork_button" onclick="var f = document.createElement('form'); f.style.display = 'none'; this.parentNode.appendChild(f); f.method = 'POST'; f.action = this.href;var s = document.createElement('input'); s.setAttribute('type', 'hidden'); s.setAttribute('name', 'authenticity_token'); s.setAttribute('value', '2d10bc5ead7c36efc020ba2c079560fb0a138d02'); f.appendChild(s);f.submit();return false;"><span><span class="icon"></span>Fork</span></a></li>
          <li class="for-hasfork" style="display:none"><a href="#" btn_class="fork" class="minibutton btn-fork " id="your_fork_button"><span><span class="icon"></span>Your Fork</span></a></li>
          <li id="pull_request_item" style="display:none"><a href="/nikodemus/esrap/pull_request/" class="minibutton btn-pull-request "><span><span class="icon"></span>Pull Request</span></a></li>
          <li><a href="#" btn_class="download" class="minibutton btn-download " id="download_button"><span><span class="icon"></span>Download Source</span></a></li>
        
      
      <li class="repostats">
        <ul class="repo-stats">
          <li class="watchers"><a href="/nikodemus/esrap/watchers" title="Watchers" class="tooltipped downwards">4</a></li>
          <li class="forks"><a href="/nikodemus/esrap/network" title="Forks" class="tooltipped downwards">1</a></li>
        </ul>
      </li>
    </ul>


        <ul class="tabs">
  <li><a href="http://github.com/nikodemus/esrap/tree/master" class="selected" highlight="repo_source">Source</a></li>
  <li><a href="http://github.com/nikodemus/esrap/commits/master" class="false" highlight="repo_commits">Commits</a></li>

  
  <li><a href="/nikodemus/esrap/network" class="false" highlight="repo_network">Network (1)</a></li>

  

  
    
    <li><a href="/nikodemus/esrap/issues" class="false" highlight="issues">Issues (0)</a></li>
  

  
    
    <li><a href="/nikodemus/esrap/downloads" class="false">Downloads (0)</a></li>
  

  
    
    <li><a href="http://wiki.github.com/nikodemus/esrap/" class="false">Wiki (1)</a></li>
  

  <li><a href="/nikodemus/esrap/graphs" class="false" highlight="repo_graphs">Graphs</a></li>

  <li class="contextswitch nochoices">
    <span class="toggle leftwards" >
      <em>Branch:</em>
      <code>master</code>
    </span>
  </li>
</ul>

<div style="display:none" id="pl-description"><p><em class="placeholder">click here to add a description</em></p></div>
<div style="display:none" id="pl-homepage"><p><em class="placeholder">click here to add a homepage</em></p></div>

<div class="subnav-bar">
  
  <ul>
    <li>
      <a href="#" class="dropdown">Branches (1)</a>
      <ul>
        
          
            <li><strong>master &#x2713;</strong></li>
            
      </ul>
    </li>
    <li>
      <a href="#" class="dropdown defunct">Tags (0)</a>
      
    </li>
  </ul>
</div>









        
    <div id="repo_details" class="metabox clearfix  ">
      <div id="repo_details_loader" class="metabox-loader" style="display:none">Sending Request&hellip;</div>

      
        
          <a href="#pledgie_box" rel="facebox" title="Brought to you by pledgie.com" class="pledgie pledgie-button for-owner tooltipped" id="activate_pledgie_button" style="display:none"><span>Enable Donations</span></a>
        
        
      

      <div id="pledgie_box" style="display:none">
        <h2>Pledgie Donations</h2>
        <form action="/nikodemus/esrap/edit/donate" method="post"><div style="margin:0;padding:0"><input name="authenticity_token" type="hidden" value="2d10bc5ead7c36efc020ba2c079560fb0a138d02" /></div>
          <dl class="form miniform">
            <dt><label>Paypal Email</label></dt>
            <dd><input type="text" id="paypal" name="paypal" /></dd>
          </dl>
          <div class="form-actions">
            
            <button type="submit" class="minibutton"><span>Activate Donations</span></button>
          </div>
        </form>
        <div class="rule"></div>
        Once activated, we'll place the following badge in your repository's detail box:
        <div style="text-align:center">
          <img alt="Pledgie_example" src="http://assets0.github.com/images/modules/pagehead/pledgie_example.jpg?7371c81fbc6b010a32fb11b42a0fc322c3c57863" />
        </div>
        This service is courtesy of <a href="http://pledgie.com">Pledgie</a>.
      </div>

      <div id="repository_description" rel="repository_description_edit">
        
          <p>Common Lisp packrat parser
            <span id="read_more" style="display:none">&mdash; <a href="#readme">Read more</a></span>
          </p>
        
      </div>
      <div id="repository_description_edit" style="display:none;" class="inline-edit">
        <form action="/nikodemus/esrap/edit/update" method="post"><div style="margin:0;padding:0"><input name="authenticity_token" type="hidden" value="2d10bc5ead7c36efc020ba2c079560fb0a138d02" /></div>
          <input type="hidden" name="field" value="repository_description">
          <input type="text" class="textfield" name="value" value="Common Lisp packrat parser">
          <div class="form-actions">
            <button class="minibutton"><span>Save</span></button> &nbsp; <a href="#" class="cancel">cancel</a>
          </div>
        </form>
      </div>

      
        
        <div class="repository-homepage" id="repository_homepage" rel="repository_homepage_edit">
          <p><a href="http://" rel="nofollow"></a></p>
        </div>
        <div id="repository_homepage_edit" style="display:none;" class="inline-edit">
          <form action="/nikodemus/esrap/edit/update" method="post"><div style="margin:0;padding:0"><input name="authenticity_token" type="hidden" value="2d10bc5ead7c36efc020ba2c079560fb0a138d02" /></div>
            <input type="hidden" name="field" value="repository_homepage">
            <input type="text" class="textfield" name="value" value="">
            <div class="form-actions">
              <button class="minibutton"><span>Save</span></button> &nbsp; <a href="#" class="cancel">cancel</a>
            </div>
          </form>
        </div>
      

      
        <div class="rule "></div>

        <div id="url_box" class="url-box">
          <ul class="clone-urls">
            <li id="private_clone_url" style="display:none"><a href="git@github.com:nikodemus/esrap.git" data-permissions="Read+Write">Private</a></li>
            
              <li id="public_clone_url"><a href="git://github.com/nikodemus/esrap.git" data-permissions="Read-Only">Read-Only</a></li>
              <li id="http_clone_url"><a href="http://github.com/nikodemus/esrap.git" data-permissions="Read-Only">HTTP Read-Only</a></li>
            
          </ul>
          <input type="text" spellcheck="false" id="url_field" class="url-field" />
                <span style="display:none" id="url_box_clippy"></span>
      <span id="clippy_tooltip_url_box_clippy" class="clippy-tooltip tooltipped" title="copy to clipboard">
      <object classid="clsid:d27cdb6e-ae6d-11cf-96b8-444553540000"
              width="14"
              height="14"
              class="clippy"
              id="clippy" >
      <param name="movie" value="/flash/clippy.swf?v5"/>
      <param name="allowScriptAccess" value="always" />
      <param name="quality" value="high" />
      <param name="scale" value="noscale" />
      <param NAME="FlashVars" value="id=url_box_clippy&amp;copied=&amp;copyto=">
      <param name="bgcolor" value="#FFFFFF">
      <param name="wmode" value="opaque">
      <embed src="/flash/clippy.swf?v5"
             width="14"
             height="14"
             name="clippy"
             quality="high"
             allowScriptAccess="always"
             type="application/x-shockwave-flash"
             pluginspage="http://www.macromedia.com/go/getflashplayer"
             FlashVars="id=url_box_clippy&amp;copied=&amp;copyto="
             bgcolor="#FFFFFF"
             wmode="opaque"
      />
      </object>
      </span>

          <p id="url_description">This URL has <strong>Read+Write</strong> access</p>
        </div>
      
    </div>


      </div><!-- /.pagehead -->

      









<script type="text/javascript">
  GitHub.currentCommitRef = "master"
  GitHub.currentRepoOwner = "nikodemus"
  GitHub.currentRepo = "esrap"
  GitHub.downloadRepo = '/nikodemus/esrap/archives/master'
  

  
</script>










  <div id="commit">
    <div class="group">
        
  <div class="envelope commit">
    <div class="human">
      
        <div class="message"><pre><a href="/nikodemus/esrap/commit/571ba67e45c42d3af1e458ff6863c1aba28270c3">initial commit</a> </pre></div>
      

      <div class="actor">
        <div class="gravatar">
          
          <img alt="" height="30" src="http://www.gravatar.com/avatar/5cd0acc03793e7944454dbb533bbfe43?s=30&amp;d=http%3A%2F%2Fgithub.com%2Fimages%2Fgravatars%2Fgravatar-30.png" width="30" />
        </div>
        <div class="name"><a href="/nikodemus">nikodemus</a> <span>(author)</span></div>
        <div class="date">
          <abbr class="relatize" title="2009-11-14 05:23:30">Sat Nov 14 05:23:30 -0800 2009</abbr>
        </div>
      </div>

      

    </div>
    <div class="machine">
      <span>c</span>ommit&nbsp;&nbsp;<a href="/nikodemus/esrap/commit/571ba67e45c42d3af1e458ff6863c1aba28270c3" hotkey="c">571ba67e45c42d3af1e458ff6863c1aba28270c3</a><br />
      <span>t</span>ree&nbsp;&nbsp;&nbsp;&nbsp;<a href="/nikodemus/esrap/tree/571ba67e45c42d3af1e458ff6863c1aba28270c3" hotkey="t">69234d4a10f6eacff6e75ab22ec78917da4ead6f</a><br />
      

    </div>
  </div>

    </div>
  </div>



  
    <div id="path">
      <b><a href="/nikodemus/esrap/tree/master">esrap</a></b> / esrap.lisp       <span style="display:none" id="clippy_3401">esrap.lisp</span>
      
      <object classid="clsid:d27cdb6e-ae6d-11cf-96b8-444553540000"
              width="110"
              height="14"
              class="clippy"
              id="clippy" >
      <param name="movie" value="/flash/clippy.swf?v5"/>
      <param name="allowScriptAccess" value="always" />
      <param name="quality" value="high" />
      <param name="scale" value="noscale" />
      <param NAME="FlashVars" value="id=clippy_3401&amp;copied=copied!&amp;copyto=copy to clipboard">
      <param name="bgcolor" value="#FFFFFF">
      <param name="wmode" value="opaque">
      <embed src="/flash/clippy.swf?v5"
             width="110"
             height="14"
             name="clippy"
             quality="high"
             allowScriptAccess="always"
             type="application/x-shockwave-flash"
             pluginspage="http://www.macromedia.com/go/getflashplayer"
             FlashVars="id=clippy_3401&amp;copied=copied!&amp;copyto=copy to clipboard"
             bgcolor="#FFFFFF"
             wmode="opaque"
      />
      </object>
      

    </div>

    <div id="files">
      <div class="file">
        <div class="meta">
          <div class="info">
            <span>100644</span>
            <span>957 lines (854 sloc)</span>
            <span>34.167 kb</span>
          </div>
          <div class="actions">
            
              <a style="display:none;" id="file-edit-link" href="#" rel="/nikodemus/esrap/file-edit/__ref__/esrap.lisp">edit</a>
            
            <a href="/nikodemus/esrap/raw/master/esrap.lisp" id="raw-url">raw</a>
            
              <a href="/nikodemus/esrap/blame/master/esrap.lisp">blame</a>
            
            <a href="/nikodemus/esrap/commits/master/esrap.lisp">history</a>
          </div>
        </div>
        
  <div class="data syntax type-cl">
    
      <table cellpadding="0" cellspacing="0">
        <tr>
          <td>
            
            <pre class="line_numbers">
<span id="LID1" rel="#L1">1</span>
<span id="LID2" rel="#L2">2</span>
<span id="LID3" rel="#L3">3</span>
<span id="LID4" rel="#L4">4</span>
<span id="LID5" rel="#L5">5</span>
<span id="LID6" rel="#L6">6</span>
<span id="LID7" rel="#L7">7</span>
<span id="LID8" rel="#L8">8</span>
<span id="LID9" rel="#L9">9</span>
<span id="LID10" rel="#L10">10</span>
<span id="LID11" rel="#L11">11</span>
<span id="LID12" rel="#L12">12</span>
<span id="LID13" rel="#L13">13</span>
<span id="LID14" rel="#L14">14</span>
<span id="LID15" rel="#L15">15</span>
<span id="LID16" rel="#L16">16</span>
<span id="LID17" rel="#L17">17</span>
<span id="LID18" rel="#L18">18</span>
<span id="LID19" rel="#L19">19</span>
<span id="LID20" rel="#L20">20</span>
<span id="LID21" rel="#L21">21</span>
<span id="LID22" rel="#L22">22</span>
<span id="LID23" rel="#L23">23</span>
<span id="LID24" rel="#L24">24</span>
<span id="LID25" rel="#L25">25</span>
<span id="LID26" rel="#L26">26</span>
<span id="LID27" rel="#L27">27</span>
<span id="LID28" rel="#L28">28</span>
<span id="LID29" rel="#L29">29</span>
<span id="LID30" rel="#L30">30</span>
<span id="LID31" rel="#L31">31</span>
<span id="LID32" rel="#L32">32</span>
<span id="LID33" rel="#L33">33</span>
<span id="LID34" rel="#L34">34</span>
<span id="LID35" rel="#L35">35</span>
<span id="LID36" rel="#L36">36</span>
<span id="LID37" rel="#L37">37</span>
<span id="LID38" rel="#L38">38</span>
<span id="LID39" rel="#L39">39</span>
<span id="LID40" rel="#L40">40</span>
<span id="LID41" rel="#L41">41</span>
<span id="LID42" rel="#L42">42</span>
<span id="LID43" rel="#L43">43</span>
<span id="LID44" rel="#L44">44</span>
<span id="LID45" rel="#L45">45</span>
<span id="LID46" rel="#L46">46</span>
<span id="LID47" rel="#L47">47</span>
<span id="LID48" rel="#L48">48</span>
<span id="LID49" rel="#L49">49</span>
<span id="LID50" rel="#L50">50</span>
<span id="LID51" rel="#L51">51</span>
<span id="LID52" rel="#L52">52</span>
<span id="LID53" rel="#L53">53</span>
<span id="LID54" rel="#L54">54</span>
<span id="LID55" rel="#L55">55</span>
<span id="LID56" rel="#L56">56</span>
<span id="LID57" rel="#L57">57</span>
<span id="LID58" rel="#L58">58</span>
<span id="LID59" rel="#L59">59</span>
<span id="LID60" rel="#L60">60</span>
<span id="LID61" rel="#L61">61</span>
<span id="LID62" rel="#L62">62</span>
<span id="LID63" rel="#L63">63</span>
<span id="LID64" rel="#L64">64</span>
<span id="LID65" rel="#L65">65</span>
<span id="LID66" rel="#L66">66</span>
<span id="LID67" rel="#L67">67</span>
<span id="LID68" rel="#L68">68</span>
<span id="LID69" rel="#L69">69</span>
<span id="LID70" rel="#L70">70</span>
<span id="LID71" rel="#L71">71</span>
<span id="LID72" rel="#L72">72</span>
<span id="LID73" rel="#L73">73</span>
<span id="LID74" rel="#L74">74</span>
<span id="LID75" rel="#L75">75</span>
<span id="LID76" rel="#L76">76</span>
<span id="LID77" rel="#L77">77</span>
<span id="LID78" rel="#L78">78</span>
<span id="LID79" rel="#L79">79</span>
<span id="LID80" rel="#L80">80</span>
<span id="LID81" rel="#L81">81</span>
<span id="LID82" rel="#L82">82</span>
<span id="LID83" rel="#L83">83</span>
<span id="LID84" rel="#L84">84</span>
<span id="LID85" rel="#L85">85</span>
<span id="LID86" rel="#L86">86</span>
<span id="LID87" rel="#L87">87</span>
<span id="LID88" rel="#L88">88</span>
<span id="LID89" rel="#L89">89</span>
<span id="LID90" rel="#L90">90</span>
<span id="LID91" rel="#L91">91</span>
<span id="LID92" rel="#L92">92</span>
<span id="LID93" rel="#L93">93</span>
<span id="LID94" rel="#L94">94</span>
<span id="LID95" rel="#L95">95</span>
<span id="LID96" rel="#L96">96</span>
<span id="LID97" rel="#L97">97</span>
<span id="LID98" rel="#L98">98</span>
<span id="LID99" rel="#L99">99</span>
<span id="LID100" rel="#L100">100</span>
<span id="LID101" rel="#L101">101</span>
<span id="LID102" rel="#L102">102</span>
<span id="LID103" rel="#L103">103</span>
<span id="LID104" rel="#L104">104</span>
<span id="LID105" rel="#L105">105</span>
<span id="LID106" rel="#L106">106</span>
<span id="LID107" rel="#L107">107</span>
<span id="LID108" rel="#L108">108</span>
<span id="LID109" rel="#L109">109</span>
<span id="LID110" rel="#L110">110</span>
<span id="LID111" rel="#L111">111</span>
<span id="LID112" rel="#L112">112</span>
<span id="LID113" rel="#L113">113</span>
<span id="LID114" rel="#L114">114</span>
<span id="LID115" rel="#L115">115</span>
<span id="LID116" rel="#L116">116</span>
<span id="LID117" rel="#L117">117</span>
<span id="LID118" rel="#L118">118</span>
<span id="LID119" rel="#L119">119</span>
<span id="LID120" rel="#L120">120</span>
<span id="LID121" rel="#L121">121</span>
<span id="LID122" rel="#L122">122</span>
<span id="LID123" rel="#L123">123</span>
<span id="LID124" rel="#L124">124</span>
<span id="LID125" rel="#L125">125</span>
<span id="LID126" rel="#L126">126</span>
<span id="LID127" rel="#L127">127</span>
<span id="LID128" rel="#L128">128</span>
<span id="LID129" rel="#L129">129</span>
<span id="LID130" rel="#L130">130</span>
<span id="LID131" rel="#L131">131</span>
<span id="LID132" rel="#L132">132</span>
<span id="LID133" rel="#L133">133</span>
<span id="LID134" rel="#L134">134</span>
<span id="LID135" rel="#L135">135</span>
<span id="LID136" rel="#L136">136</span>
<span id="LID137" rel="#L137">137</span>
<span id="LID138" rel="#L138">138</span>
<span id="LID139" rel="#L139">139</span>
<span id="LID140" rel="#L140">140</span>
<span id="LID141" rel="#L141">141</span>
<span id="LID142" rel="#L142">142</span>
<span id="LID143" rel="#L143">143</span>
<span id="LID144" rel="#L144">144</span>
<span id="LID145" rel="#L145">145</span>
<span id="LID146" rel="#L146">146</span>
<span id="LID147" rel="#L147">147</span>
<span id="LID148" rel="#L148">148</span>
<span id="LID149" rel="#L149">149</span>
<span id="LID150" rel="#L150">150</span>
<span id="LID151" rel="#L151">151</span>
<span id="LID152" rel="#L152">152</span>
<span id="LID153" rel="#L153">153</span>
<span id="LID154" rel="#L154">154</span>
<span id="LID155" rel="#L155">155</span>
<span id="LID156" rel="#L156">156</span>
<span id="LID157" rel="#L157">157</span>
<span id="LID158" rel="#L158">158</span>
<span id="LID159" rel="#L159">159</span>
<span id="LID160" rel="#L160">160</span>
<span id="LID161" rel="#L161">161</span>
<span id="LID162" rel="#L162">162</span>
<span id="LID163" rel="#L163">163</span>
<span id="LID164" rel="#L164">164</span>
<span id="LID165" rel="#L165">165</span>
<span id="LID166" rel="#L166">166</span>
<span id="LID167" rel="#L167">167</span>
<span id="LID168" rel="#L168">168</span>
<span id="LID169" rel="#L169">169</span>
<span id="LID170" rel="#L170">170</span>
<span id="LID171" rel="#L171">171</span>
<span id="LID172" rel="#L172">172</span>
<span id="LID173" rel="#L173">173</span>
<span id="LID174" rel="#L174">174</span>
<span id="LID175" rel="#L175">175</span>
<span id="LID176" rel="#L176">176</span>
<span id="LID177" rel="#L177">177</span>
<span id="LID178" rel="#L178">178</span>
<span id="LID179" rel="#L179">179</span>
<span id="LID180" rel="#L180">180</span>
<span id="LID181" rel="#L181">181</span>
<span id="LID182" rel="#L182">182</span>
<span id="LID183" rel="#L183">183</span>
<span id="LID184" rel="#L184">184</span>
<span id="LID185" rel="#L185">185</span>
<span id="LID186" rel="#L186">186</span>
<span id="LID187" rel="#L187">187</span>
<span id="LID188" rel="#L188">188</span>
<span id="LID189" rel="#L189">189</span>
<span id="LID190" rel="#L190">190</span>
<span id="LID191" rel="#L191">191</span>
<span id="LID192" rel="#L192">192</span>
<span id="LID193" rel="#L193">193</span>
<span id="LID194" rel="#L194">194</span>
<span id="LID195" rel="#L195">195</span>
<span id="LID196" rel="#L196">196</span>
<span id="LID197" rel="#L197">197</span>
<span id="LID198" rel="#L198">198</span>
<span id="LID199" rel="#L199">199</span>
<span id="LID200" rel="#L200">200</span>
<span id="LID201" rel="#L201">201</span>
<span id="LID202" rel="#L202">202</span>
<span id="LID203" rel="#L203">203</span>
<span id="LID204" rel="#L204">204</span>
<span id="LID205" rel="#L205">205</span>
<span id="LID206" rel="#L206">206</span>
<span id="LID207" rel="#L207">207</span>
<span id="LID208" rel="#L208">208</span>
<span id="LID209" rel="#L209">209</span>
<span id="LID210" rel="#L210">210</span>
<span id="LID211" rel="#L211">211</span>
<span id="LID212" rel="#L212">212</span>
<span id="LID213" rel="#L213">213</span>
<span id="LID214" rel="#L214">214</span>
<span id="LID215" rel="#L215">215</span>
<span id="LID216" rel="#L216">216</span>
<span id="LID217" rel="#L217">217</span>
<span id="LID218" rel="#L218">218</span>
<span id="LID219" rel="#L219">219</span>
<span id="LID220" rel="#L220">220</span>
<span id="LID221" rel="#L221">221</span>
<span id="LID222" rel="#L222">222</span>
<span id="LID223" rel="#L223">223</span>
<span id="LID224" rel="#L224">224</span>
<span id="LID225" rel="#L225">225</span>
<span id="LID226" rel="#L226">226</span>
<span id="LID227" rel="#L227">227</span>
<span id="LID228" rel="#L228">228</span>
<span id="LID229" rel="#L229">229</span>
<span id="LID230" rel="#L230">230</span>
<span id="LID231" rel="#L231">231</span>
<span id="LID232" rel="#L232">232</span>
<span id="LID233" rel="#L233">233</span>
<span id="LID234" rel="#L234">234</span>
<span id="LID235" rel="#L235">235</span>
<span id="LID236" rel="#L236">236</span>
<span id="LID237" rel="#L237">237</span>
<span id="LID238" rel="#L238">238</span>
<span id="LID239" rel="#L239">239</span>
<span id="LID240" rel="#L240">240</span>
<span id="LID241" rel="#L241">241</span>
<span id="LID242" rel="#L242">242</span>
<span id="LID243" rel="#L243">243</span>
<span id="LID244" rel="#L244">244</span>
<span id="LID245" rel="#L245">245</span>
<span id="LID246" rel="#L246">246</span>
<span id="LID247" rel="#L247">247</span>
<span id="LID248" rel="#L248">248</span>
<span id="LID249" rel="#L249">249</span>
<span id="LID250" rel="#L250">250</span>
<span id="LID251" rel="#L251">251</span>
<span id="LID252" rel="#L252">252</span>
<span id="LID253" rel="#L253">253</span>
<span id="LID254" rel="#L254">254</span>
<span id="LID255" rel="#L255">255</span>
<span id="LID256" rel="#L256">256</span>
<span id="LID257" rel="#L257">257</span>
<span id="LID258" rel="#L258">258</span>
<span id="LID259" rel="#L259">259</span>
<span id="LID260" rel="#L260">260</span>
<span id="LID261" rel="#L261">261</span>
<span id="LID262" rel="#L262">262</span>
<span id="LID263" rel="#L263">263</span>
<span id="LID264" rel="#L264">264</span>
<span id="LID265" rel="#L265">265</span>
<span id="LID266" rel="#L266">266</span>
<span id="LID267" rel="#L267">267</span>
<span id="LID268" rel="#L268">268</span>
<span id="LID269" rel="#L269">269</span>
<span id="LID270" rel="#L270">270</span>
<span id="LID271" rel="#L271">271</span>
<span id="LID272" rel="#L272">272</span>
<span id="LID273" rel="#L273">273</span>
<span id="LID274" rel="#L274">274</span>
<span id="LID275" rel="#L275">275</span>
<span id="LID276" rel="#L276">276</span>
<span id="LID277" rel="#L277">277</span>
<span id="LID278" rel="#L278">278</span>
<span id="LID279" rel="#L279">279</span>
<span id="LID280" rel="#L280">280</span>
<span id="LID281" rel="#L281">281</span>
<span id="LID282" rel="#L282">282</span>
<span id="LID283" rel="#L283">283</span>
<span id="LID284" rel="#L284">284</span>
<span id="LID285" rel="#L285">285</span>
<span id="LID286" rel="#L286">286</span>
<span id="LID287" rel="#L287">287</span>
<span id="LID288" rel="#L288">288</span>
<span id="LID289" rel="#L289">289</span>
<span id="LID290" rel="#L290">290</span>
<span id="LID291" rel="#L291">291</span>
<span id="LID292" rel="#L292">292</span>
<span id="LID293" rel="#L293">293</span>
<span id="LID294" rel="#L294">294</span>
<span id="LID295" rel="#L295">295</span>
<span id="LID296" rel="#L296">296</span>
<span id="LID297" rel="#L297">297</span>
<span id="LID298" rel="#L298">298</span>
<span id="LID299" rel="#L299">299</span>
<span id="LID300" rel="#L300">300</span>
<span id="LID301" rel="#L301">301</span>
<span id="LID302" rel="#L302">302</span>
<span id="LID303" rel="#L303">303</span>
<span id="LID304" rel="#L304">304</span>
<span id="LID305" rel="#L305">305</span>
<span id="LID306" rel="#L306">306</span>
<span id="LID307" rel="#L307">307</span>
<span id="LID308" rel="#L308">308</span>
<span id="LID309" rel="#L309">309</span>
<span id="LID310" rel="#L310">310</span>
<span id="LID311" rel="#L311">311</span>
<span id="LID312" rel="#L312">312</span>
<span id="LID313" rel="#L313">313</span>
<span id="LID314" rel="#L314">314</span>
<span id="LID315" rel="#L315">315</span>
<span id="LID316" rel="#L316">316</span>
<span id="LID317" rel="#L317">317</span>
<span id="LID318" rel="#L318">318</span>
<span id="LID319" rel="#L319">319</span>
<span id="LID320" rel="#L320">320</span>
<span id="LID321" rel="#L321">321</span>
<span id="LID322" rel="#L322">322</span>
<span id="LID323" rel="#L323">323</span>
<span id="LID324" rel="#L324">324</span>
<span id="LID325" rel="#L325">325</span>
<span id="LID326" rel="#L326">326</span>
<span id="LID327" rel="#L327">327</span>
<span id="LID328" rel="#L328">328</span>
<span id="LID329" rel="#L329">329</span>
<span id="LID330" rel="#L330">330</span>
<span id="LID331" rel="#L331">331</span>
<span id="LID332" rel="#L332">332</span>
<span id="LID333" rel="#L333">333</span>
<span id="LID334" rel="#L334">334</span>
<span id="LID335" rel="#L335">335</span>
<span id="LID336" rel="#L336">336</span>
<span id="LID337" rel="#L337">337</span>
<span id="LID338" rel="#L338">338</span>
<span id="LID339" rel="#L339">339</span>
<span id="LID340" rel="#L340">340</span>
<span id="LID341" rel="#L341">341</span>
<span id="LID342" rel="#L342">342</span>
<span id="LID343" rel="#L343">343</span>
<span id="LID344" rel="#L344">344</span>
<span id="LID345" rel="#L345">345</span>
<span id="LID346" rel="#L346">346</span>
<span id="LID347" rel="#L347">347</span>
<span id="LID348" rel="#L348">348</span>
<span id="LID349" rel="#L349">349</span>
<span id="LID350" rel="#L350">350</span>
<span id="LID351" rel="#L351">351</span>
<span id="LID352" rel="#L352">352</span>
<span id="LID353" rel="#L353">353</span>
<span id="LID354" rel="#L354">354</span>
<span id="LID355" rel="#L355">355</span>
<span id="LID356" rel="#L356">356</span>
<span id="LID357" rel="#L357">357</span>
<span id="LID358" rel="#L358">358</span>
<span id="LID359" rel="#L359">359</span>
<span id="LID360" rel="#L360">360</span>
<span id="LID361" rel="#L361">361</span>
<span id="LID362" rel="#L362">362</span>
<span id="LID363" rel="#L363">363</span>
<span id="LID364" rel="#L364">364</span>
<span id="LID365" rel="#L365">365</span>
<span id="LID366" rel="#L366">366</span>
<span id="LID367" rel="#L367">367</span>
<span id="LID368" rel="#L368">368</span>
<span id="LID369" rel="#L369">369</span>
<span id="LID370" rel="#L370">370</span>
<span id="LID371" rel="#L371">371</span>
<span id="LID372" rel="#L372">372</span>
<span id="LID373" rel="#L373">373</span>
<span id="LID374" rel="#L374">374</span>
<span id="LID375" rel="#L375">375</span>
<span id="LID376" rel="#L376">376</span>
<span id="LID377" rel="#L377">377</span>
<span id="LID378" rel="#L378">378</span>
<span id="LID379" rel="#L379">379</span>
<span id="LID380" rel="#L380">380</span>
<span id="LID381" rel="#L381">381</span>
<span id="LID382" rel="#L382">382</span>
<span id="LID383" rel="#L383">383</span>
<span id="LID384" rel="#L384">384</span>
<span id="LID385" rel="#L385">385</span>
<span id="LID386" rel="#L386">386</span>
<span id="LID387" rel="#L387">387</span>
<span id="LID388" rel="#L388">388</span>
<span id="LID389" rel="#L389">389</span>
<span id="LID390" rel="#L390">390</span>
<span id="LID391" rel="#L391">391</span>
<span id="LID392" rel="#L392">392</span>
<span id="LID393" rel="#L393">393</span>
<span id="LID394" rel="#L394">394</span>
<span id="LID395" rel="#L395">395</span>
<span id="LID396" rel="#L396">396</span>
<span id="LID397" rel="#L397">397</span>
<span id="LID398" rel="#L398">398</span>
<span id="LID399" rel="#L399">399</span>
<span id="LID400" rel="#L400">400</span>
<span id="LID401" rel="#L401">401</span>
<span id="LID402" rel="#L402">402</span>
<span id="LID403" rel="#L403">403</span>
<span id="LID404" rel="#L404">404</span>
<span id="LID405" rel="#L405">405</span>
<span id="LID406" rel="#L406">406</span>
<span id="LID407" rel="#L407">407</span>
<span id="LID408" rel="#L408">408</span>
<span id="LID409" rel="#L409">409</span>
<span id="LID410" rel="#L410">410</span>
<span id="LID411" rel="#L411">411</span>
<span id="LID412" rel="#L412">412</span>
<span id="LID413" rel="#L413">413</span>
<span id="LID414" rel="#L414">414</span>
<span id="LID415" rel="#L415">415</span>
<span id="LID416" rel="#L416">416</span>
<span id="LID417" rel="#L417">417</span>
<span id="LID418" rel="#L418">418</span>
<span id="LID419" rel="#L419">419</span>
<span id="LID420" rel="#L420">420</span>
<span id="LID421" rel="#L421">421</span>
<span id="LID422" rel="#L422">422</span>
<span id="LID423" rel="#L423">423</span>
<span id="LID424" rel="#L424">424</span>
<span id="LID425" rel="#L425">425</span>
<span id="LID426" rel="#L426">426</span>
<span id="LID427" rel="#L427">427</span>
<span id="LID428" rel="#L428">428</span>
<span id="LID429" rel="#L429">429</span>
<span id="LID430" rel="#L430">430</span>
<span id="LID431" rel="#L431">431</span>
<span id="LID432" rel="#L432">432</span>
<span id="LID433" rel="#L433">433</span>
<span id="LID434" rel="#L434">434</span>
<span id="LID435" rel="#L435">435</span>
<span id="LID436" rel="#L436">436</span>
<span id="LID437" rel="#L437">437</span>
<span id="LID438" rel="#L438">438</span>
<span id="LID439" rel="#L439">439</span>
<span id="LID440" rel="#L440">440</span>
<span id="LID441" rel="#L441">441</span>
<span id="LID442" rel="#L442">442</span>
<span id="LID443" rel="#L443">443</span>
<span id="LID444" rel="#L444">444</span>
<span id="LID445" rel="#L445">445</span>
<span id="LID446" rel="#L446">446</span>
<span id="LID447" rel="#L447">447</span>
<span id="LID448" rel="#L448">448</span>
<span id="LID449" rel="#L449">449</span>
<span id="LID450" rel="#L450">450</span>
<span id="LID451" rel="#L451">451</span>
<span id="LID452" rel="#L452">452</span>
<span id="LID453" rel="#L453">453</span>
<span id="LID454" rel="#L454">454</span>
<span id="LID455" rel="#L455">455</span>
<span id="LID456" rel="#L456">456</span>
<span id="LID457" rel="#L457">457</span>
<span id="LID458" rel="#L458">458</span>
<span id="LID459" rel="#L459">459</span>
<span id="LID460" rel="#L460">460</span>
<span id="LID461" rel="#L461">461</span>
<span id="LID462" rel="#L462">462</span>
<span id="LID463" rel="#L463">463</span>
<span id="LID464" rel="#L464">464</span>
<span id="LID465" rel="#L465">465</span>
<span id="LID466" rel="#L466">466</span>
<span id="LID467" rel="#L467">467</span>
<span id="LID468" rel="#L468">468</span>
<span id="LID469" rel="#L469">469</span>
<span id="LID470" rel="#L470">470</span>
<span id="LID471" rel="#L471">471</span>
<span id="LID472" rel="#L472">472</span>
<span id="LID473" rel="#L473">473</span>
<span id="LID474" rel="#L474">474</span>
<span id="LID475" rel="#L475">475</span>
<span id="LID476" rel="#L476">476</span>
<span id="LID477" rel="#L477">477</span>
<span id="LID478" rel="#L478">478</span>
<span id="LID479" rel="#L479">479</span>
<span id="LID480" rel="#L480">480</span>
<span id="LID481" rel="#L481">481</span>
<span id="LID482" rel="#L482">482</span>
<span id="LID483" rel="#L483">483</span>
<span id="LID484" rel="#L484">484</span>
<span id="LID485" rel="#L485">485</span>
<span id="LID486" rel="#L486">486</span>
<span id="LID487" rel="#L487">487</span>
<span id="LID488" rel="#L488">488</span>
<span id="LID489" rel="#L489">489</span>
<span id="LID490" rel="#L490">490</span>
<span id="LID491" rel="#L491">491</span>
<span id="LID492" rel="#L492">492</span>
<span id="LID493" rel="#L493">493</span>
<span id="LID494" rel="#L494">494</span>
<span id="LID495" rel="#L495">495</span>
<span id="LID496" rel="#L496">496</span>
<span id="LID497" rel="#L497">497</span>
<span id="LID498" rel="#L498">498</span>
<span id="LID499" rel="#L499">499</span>
<span id="LID500" rel="#L500">500</span>
<span id="LID501" rel="#L501">501</span>
<span id="LID502" rel="#L502">502</span>
<span id="LID503" rel="#L503">503</span>
<span id="LID504" rel="#L504">504</span>
<span id="LID505" rel="#L505">505</span>
<span id="LID506" rel="#L506">506</span>
<span id="LID507" rel="#L507">507</span>
<span id="LID508" rel="#L508">508</span>
<span id="LID509" rel="#L509">509</span>
<span id="LID510" rel="#L510">510</span>
<span id="LID511" rel="#L511">511</span>
<span id="LID512" rel="#L512">512</span>
<span id="LID513" rel="#L513">513</span>
<span id="LID514" rel="#L514">514</span>
<span id="LID515" rel="#L515">515</span>
<span id="LID516" rel="#L516">516</span>
<span id="LID517" rel="#L517">517</span>
<span id="LID518" rel="#L518">518</span>
<span id="LID519" rel="#L519">519</span>
<span id="LID520" rel="#L520">520</span>
<span id="LID521" rel="#L521">521</span>
<span id="LID522" rel="#L522">522</span>
<span id="LID523" rel="#L523">523</span>
<span id="LID524" rel="#L524">524</span>
<span id="LID525" rel="#L525">525</span>
<span id="LID526" rel="#L526">526</span>
<span id="LID527" rel="#L527">527</span>
<span id="LID528" rel="#L528">528</span>
<span id="LID529" rel="#L529">529</span>
<span id="LID530" rel="#L530">530</span>
<span id="LID531" rel="#L531">531</span>
<span id="LID532" rel="#L532">532</span>
<span id="LID533" rel="#L533">533</span>
<span id="LID534" rel="#L534">534</span>
<span id="LID535" rel="#L535">535</span>
<span id="LID536" rel="#L536">536</span>
<span id="LID537" rel="#L537">537</span>
<span id="LID538" rel="#L538">538</span>
<span id="LID539" rel="#L539">539</span>
<span id="LID540" rel="#L540">540</span>
<span id="LID541" rel="#L541">541</span>
<span id="LID542" rel="#L542">542</span>
<span id="LID543" rel="#L543">543</span>
<span id="LID544" rel="#L544">544</span>
<span id="LID545" rel="#L545">545</span>
<span id="LID546" rel="#L546">546</span>
<span id="LID547" rel="#L547">547</span>
<span id="LID548" rel="#L548">548</span>
<span id="LID549" rel="#L549">549</span>
<span id="LID550" rel="#L550">550</span>
<span id="LID551" rel="#L551">551</span>
<span id="LID552" rel="#L552">552</span>
<span id="LID553" rel="#L553">553</span>
<span id="LID554" rel="#L554">554</span>
<span id="LID555" rel="#L555">555</span>
<span id="LID556" rel="#L556">556</span>
<span id="LID557" rel="#L557">557</span>
<span id="LID558" rel="#L558">558</span>
<span id="LID559" rel="#L559">559</span>
<span id="LID560" rel="#L560">560</span>
<span id="LID561" rel="#L561">561</span>
<span id="LID562" rel="#L562">562</span>
<span id="LID563" rel="#L563">563</span>
<span id="LID564" rel="#L564">564</span>
<span id="LID565" rel="#L565">565</span>
<span id="LID566" rel="#L566">566</span>
<span id="LID567" rel="#L567">567</span>
<span id="LID568" rel="#L568">568</span>
<span id="LID569" rel="#L569">569</span>
<span id="LID570" rel="#L570">570</span>
<span id="LID571" rel="#L571">571</span>
<span id="LID572" rel="#L572">572</span>
<span id="LID573" rel="#L573">573</span>
<span id="LID574" rel="#L574">574</span>
<span id="LID575" rel="#L575">575</span>
<span id="LID576" rel="#L576">576</span>
<span id="LID577" rel="#L577">577</span>
<span id="LID578" rel="#L578">578</span>
<span id="LID579" rel="#L579">579</span>
<span id="LID580" rel="#L580">580</span>
<span id="LID581" rel="#L581">581</span>
<span id="LID582" rel="#L582">582</span>
<span id="LID583" rel="#L583">583</span>
<span id="LID584" rel="#L584">584</span>
<span id="LID585" rel="#L585">585</span>
<span id="LID586" rel="#L586">586</span>
<span id="LID587" rel="#L587">587</span>
<span id="LID588" rel="#L588">588</span>
<span id="LID589" rel="#L589">589</span>
<span id="LID590" rel="#L590">590</span>
<span id="LID591" rel="#L591">591</span>
<span id="LID592" rel="#L592">592</span>
<span id="LID593" rel="#L593">593</span>
<span id="LID594" rel="#L594">594</span>
<span id="LID595" rel="#L595">595</span>
<span id="LID596" rel="#L596">596</span>
<span id="LID597" rel="#L597">597</span>
<span id="LID598" rel="#L598">598</span>
<span id="LID599" rel="#L599">599</span>
<span id="LID600" rel="#L600">600</span>
<span id="LID601" rel="#L601">601</span>
<span id="LID602" rel="#L602">602</span>
<span id="LID603" rel="#L603">603</span>
<span id="LID604" rel="#L604">604</span>
<span id="LID605" rel="#L605">605</span>
<span id="LID606" rel="#L606">606</span>
<span id="LID607" rel="#L607">607</span>
<span id="LID608" rel="#L608">608</span>
<span id="LID609" rel="#L609">609</span>
<span id="LID610" rel="#L610">610</span>
<span id="LID611" rel="#L611">611</span>
<span id="LID612" rel="#L612">612</span>
<span id="LID613" rel="#L613">613</span>
<span id="LID614" rel="#L614">614</span>
<span id="LID615" rel="#L615">615</span>
<span id="LID616" rel="#L616">616</span>
<span id="LID617" rel="#L617">617</span>
<span id="LID618" rel="#L618">618</span>
<span id="LID619" rel="#L619">619</span>
<span id="LID620" rel="#L620">620</span>
<span id="LID621" rel="#L621">621</span>
<span id="LID622" rel="#L622">622</span>
<span id="LID623" rel="#L623">623</span>
<span id="LID624" rel="#L624">624</span>
<span id="LID625" rel="#L625">625</span>
<span id="LID626" rel="#L626">626</span>
<span id="LID627" rel="#L627">627</span>
<span id="LID628" rel="#L628">628</span>
<span id="LID629" rel="#L629">629</span>
<span id="LID630" rel="#L630">630</span>
<span id="LID631" rel="#L631">631</span>
<span id="LID632" rel="#L632">632</span>
<span id="LID633" rel="#L633">633</span>
<span id="LID634" rel="#L634">634</span>
<span id="LID635" rel="#L635">635</span>
<span id="LID636" rel="#L636">636</span>
<span id="LID637" rel="#L637">637</span>
<span id="LID638" rel="#L638">638</span>
<span id="LID639" rel="#L639">639</span>
<span id="LID640" rel="#L640">640</span>
<span id="LID641" rel="#L641">641</span>
<span id="LID642" rel="#L642">642</span>
<span id="LID643" rel="#L643">643</span>
<span id="LID644" rel="#L644">644</span>
<span id="LID645" rel="#L645">645</span>
<span id="LID646" rel="#L646">646</span>
<span id="LID647" rel="#L647">647</span>
<span id="LID648" rel="#L648">648</span>
<span id="LID649" rel="#L649">649</span>
<span id="LID650" rel="#L650">650</span>
<span id="LID651" rel="#L651">651</span>
<span id="LID652" rel="#L652">652</span>
<span id="LID653" rel="#L653">653</span>
<span id="LID654" rel="#L654">654</span>
<span id="LID655" rel="#L655">655</span>
<span id="LID656" rel="#L656">656</span>
<span id="LID657" rel="#L657">657</span>
<span id="LID658" rel="#L658">658</span>
<span id="LID659" rel="#L659">659</span>
<span id="LID660" rel="#L660">660</span>
<span id="LID661" rel="#L661">661</span>
<span id="LID662" rel="#L662">662</span>
<span id="LID663" rel="#L663">663</span>
<span id="LID664" rel="#L664">664</span>
<span id="LID665" rel="#L665">665</span>
<span id="LID666" rel="#L666">666</span>
<span id="LID667" rel="#L667">667</span>
<span id="LID668" rel="#L668">668</span>
<span id="LID669" rel="#L669">669</span>
<span id="LID670" rel="#L670">670</span>
<span id="LID671" rel="#L671">671</span>
<span id="LID672" rel="#L672">672</span>
<span id="LID673" rel="#L673">673</span>
<span id="LID674" rel="#L674">674</span>
<span id="LID675" rel="#L675">675</span>
<span id="LID676" rel="#L676">676</span>
<span id="LID677" rel="#L677">677</span>
<span id="LID678" rel="#L678">678</span>
<span id="LID679" rel="#L679">679</span>
<span id="LID680" rel="#L680">680</span>
<span id="LID681" rel="#L681">681</span>
<span id="LID682" rel="#L682">682</span>
<span id="LID683" rel="#L683">683</span>
<span id="LID684" rel="#L684">684</span>
<span id="LID685" rel="#L685">685</span>
<span id="LID686" rel="#L686">686</span>
<span id="LID687" rel="#L687">687</span>
<span id="LID688" rel="#L688">688</span>
<span id="LID689" rel="#L689">689</span>
<span id="LID690" rel="#L690">690</span>
<span id="LID691" rel="#L691">691</span>
<span id="LID692" rel="#L692">692</span>
<span id="LID693" rel="#L693">693</span>
<span id="LID694" rel="#L694">694</span>
<span id="LID695" rel="#L695">695</span>
<span id="LID696" rel="#L696">696</span>
<span id="LID697" rel="#L697">697</span>
<span id="LID698" rel="#L698">698</span>
<span id="LID699" rel="#L699">699</span>
<span id="LID700" rel="#L700">700</span>
<span id="LID701" rel="#L701">701</span>
<span id="LID702" rel="#L702">702</span>
<span id="LID703" rel="#L703">703</span>
<span id="LID704" rel="#L704">704</span>
<span id="LID705" rel="#L705">705</span>
<span id="LID706" rel="#L706">706</span>
<span id="LID707" rel="#L707">707</span>
<span id="LID708" rel="#L708">708</span>
<span id="LID709" rel="#L709">709</span>
<span id="LID710" rel="#L710">710</span>
<span id="LID711" rel="#L711">711</span>
<span id="LID712" rel="#L712">712</span>
<span id="LID713" rel="#L713">713</span>
<span id="LID714" rel="#L714">714</span>
<span id="LID715" rel="#L715">715</span>
<span id="LID716" rel="#L716">716</span>
<span id="LID717" rel="#L717">717</span>
<span id="LID718" rel="#L718">718</span>
<span id="LID719" rel="#L719">719</span>
<span id="LID720" rel="#L720">720</span>
<span id="LID721" rel="#L721">721</span>
<span id="LID722" rel="#L722">722</span>
<span id="LID723" rel="#L723">723</span>
<span id="LID724" rel="#L724">724</span>
<span id="LID725" rel="#L725">725</span>
<span id="LID726" rel="#L726">726</span>
<span id="LID727" rel="#L727">727</span>
<span id="LID728" rel="#L728">728</span>
<span id="LID729" rel="#L729">729</span>
<span id="LID730" rel="#L730">730</span>
<span id="LID731" rel="#L731">731</span>
<span id="LID732" rel="#L732">732</span>
<span id="LID733" rel="#L733">733</span>
<span id="LID734" rel="#L734">734</span>
<span id="LID735" rel="#L735">735</span>
<span id="LID736" rel="#L736">736</span>
<span id="LID737" rel="#L737">737</span>
<span id="LID738" rel="#L738">738</span>
<span id="LID739" rel="#L739">739</span>
<span id="LID740" rel="#L740">740</span>
<span id="LID741" rel="#L741">741</span>
<span id="LID742" rel="#L742">742</span>
<span id="LID743" rel="#L743">743</span>
<span id="LID744" rel="#L744">744</span>
<span id="LID745" rel="#L745">745</span>
<span id="LID746" rel="#L746">746</span>
<span id="LID747" rel="#L747">747</span>
<span id="LID748" rel="#L748">748</span>
<span id="LID749" rel="#L749">749</span>
<span id="LID750" rel="#L750">750</span>
<span id="LID751" rel="#L751">751</span>
<span id="LID752" rel="#L752">752</span>
<span id="LID753" rel="#L753">753</span>
<span id="LID754" rel="#L754">754</span>
<span id="LID755" rel="#L755">755</span>
<span id="LID756" rel="#L756">756</span>
<span id="LID757" rel="#L757">757</span>
<span id="LID758" rel="#L758">758</span>
<span id="LID759" rel="#L759">759</span>
<span id="LID760" rel="#L760">760</span>
<span id="LID761" rel="#L761">761</span>
<span id="LID762" rel="#L762">762</span>
<span id="LID763" rel="#L763">763</span>
<span id="LID764" rel="#L764">764</span>
<span id="LID765" rel="#L765">765</span>
<span id="LID766" rel="#L766">766</span>
<span id="LID767" rel="#L767">767</span>
<span id="LID768" rel="#L768">768</span>
<span id="LID769" rel="#L769">769</span>
<span id="LID770" rel="#L770">770</span>
<span id="LID771" rel="#L771">771</span>
<span id="LID772" rel="#L772">772</span>
<span id="LID773" rel="#L773">773</span>
<span id="LID774" rel="#L774">774</span>
<span id="LID775" rel="#L775">775</span>
<span id="LID776" rel="#L776">776</span>
<span id="LID777" rel="#L777">777</span>
<span id="LID778" rel="#L778">778</span>
<span id="LID779" rel="#L779">779</span>
<span id="LID780" rel="#L780">780</span>
<span id="LID781" rel="#L781">781</span>
<span id="LID782" rel="#L782">782</span>
<span id="LID783" rel="#L783">783</span>
<span id="LID784" rel="#L784">784</span>
<span id="LID785" rel="#L785">785</span>
<span id="LID786" rel="#L786">786</span>
<span id="LID787" rel="#L787">787</span>
<span id="LID788" rel="#L788">788</span>
<span id="LID789" rel="#L789">789</span>
<span id="LID790" rel="#L790">790</span>
<span id="LID791" rel="#L791">791</span>
<span id="LID792" rel="#L792">792</span>
<span id="LID793" rel="#L793">793</span>
<span id="LID794" rel="#L794">794</span>
<span id="LID795" rel="#L795">795</span>
<span id="LID796" rel="#L796">796</span>
<span id="LID797" rel="#L797">797</span>
<span id="LID798" rel="#L798">798</span>
<span id="LID799" rel="#L799">799</span>
<span id="LID800" rel="#L800">800</span>
<span id="LID801" rel="#L801">801</span>
<span id="LID802" rel="#L802">802</span>
<span id="LID803" rel="#L803">803</span>
<span id="LID804" rel="#L804">804</span>
<span id="LID805" rel="#L805">805</span>
<span id="LID806" rel="#L806">806</span>
<span id="LID807" rel="#L807">807</span>
<span id="LID808" rel="#L808">808</span>
<span id="LID809" rel="#L809">809</span>
<span id="LID810" rel="#L810">810</span>
<span id="LID811" rel="#L811">811</span>
<span id="LID812" rel="#L812">812</span>
<span id="LID813" rel="#L813">813</span>
<span id="LID814" rel="#L814">814</span>
<span id="LID815" rel="#L815">815</span>
<span id="LID816" rel="#L816">816</span>
<span id="LID817" rel="#L817">817</span>
<span id="LID818" rel="#L818">818</span>
<span id="LID819" rel="#L819">819</span>
<span id="LID820" rel="#L820">820</span>
<span id="LID821" rel="#L821">821</span>
<span id="LID822" rel="#L822">822</span>
<span id="LID823" rel="#L823">823</span>
<span id="LID824" rel="#L824">824</span>
<span id="LID825" rel="#L825">825</span>
<span id="LID826" rel="#L826">826</span>
<span id="LID827" rel="#L827">827</span>
<span id="LID828" rel="#L828">828</span>
<span id="LID829" rel="#L829">829</span>
<span id="LID830" rel="#L830">830</span>
<span id="LID831" rel="#L831">831</span>
<span id="LID832" rel="#L832">832</span>
<span id="LID833" rel="#L833">833</span>
<span id="LID834" rel="#L834">834</span>
<span id="LID835" rel="#L835">835</span>
<span id="LID836" rel="#L836">836</span>
<span id="LID837" rel="#L837">837</span>
<span id="LID838" rel="#L838">838</span>
<span id="LID839" rel="#L839">839</span>
<span id="LID840" rel="#L840">840</span>
<span id="LID841" rel="#L841">841</span>
<span id="LID842" rel="#L842">842</span>
<span id="LID843" rel="#L843">843</span>
<span id="LID844" rel="#L844">844</span>
<span id="LID845" rel="#L845">845</span>
<span id="LID846" rel="#L846">846</span>
<span id="LID847" rel="#L847">847</span>
<span id="LID848" rel="#L848">848</span>
<span id="LID849" rel="#L849">849</span>
<span id="LID850" rel="#L850">850</span>
<span id="LID851" rel="#L851">851</span>
<span id="LID852" rel="#L852">852</span>
<span id="LID853" rel="#L853">853</span>
<span id="LID854" rel="#L854">854</span>
<span id="LID855" rel="#L855">855</span>
<span id="LID856" rel="#L856">856</span>
<span id="LID857" rel="#L857">857</span>
<span id="LID858" rel="#L858">858</span>
<span id="LID859" rel="#L859">859</span>
<span id="LID860" rel="#L860">860</span>
<span id="LID861" rel="#L861">861</span>
<span id="LID862" rel="#L862">862</span>
<span id="LID863" rel="#L863">863</span>
<span id="LID864" rel="#L864">864</span>
<span id="LID865" rel="#L865">865</span>
<span id="LID866" rel="#L866">866</span>
<span id="LID867" rel="#L867">867</span>
<span id="LID868" rel="#L868">868</span>
<span id="LID869" rel="#L869">869</span>
<span id="LID870" rel="#L870">870</span>
<span id="LID871" rel="#L871">871</span>
<span id="LID872" rel="#L872">872</span>
<span id="LID873" rel="#L873">873</span>
<span id="LID874" rel="#L874">874</span>
<span id="LID875" rel="#L875">875</span>
<span id="LID876" rel="#L876">876</span>
<span id="LID877" rel="#L877">877</span>
<span id="LID878" rel="#L878">878</span>
<span id="LID879" rel="#L879">879</span>
<span id="LID880" rel="#L880">880</span>
<span id="LID881" rel="#L881">881</span>
<span id="LID882" rel="#L882">882</span>
<span id="LID883" rel="#L883">883</span>
<span id="LID884" rel="#L884">884</span>
<span id="LID885" rel="#L885">885</span>
<span id="LID886" rel="#L886">886</span>
<span id="LID887" rel="#L887">887</span>
<span id="LID888" rel="#L888">888</span>
<span id="LID889" rel="#L889">889</span>
<span id="LID890" rel="#L890">890</span>
<span id="LID891" rel="#L891">891</span>
<span id="LID892" rel="#L892">892</span>
<span id="LID893" rel="#L893">893</span>
<span id="LID894" rel="#L894">894</span>
<span id="LID895" rel="#L895">895</span>
<span id="LID896" rel="#L896">896</span>
<span id="LID897" rel="#L897">897</span>
<span id="LID898" rel="#L898">898</span>
<span id="LID899" rel="#L899">899</span>
<span id="LID900" rel="#L900">900</span>
<span id="LID901" rel="#L901">901</span>
<span id="LID902" rel="#L902">902</span>
<span id="LID903" rel="#L903">903</span>
<span id="LID904" rel="#L904">904</span>
<span id="LID905" rel="#L905">905</span>
<span id="LID906" rel="#L906">906</span>
<span id="LID907" rel="#L907">907</span>
<span id="LID908" rel="#L908">908</span>
<span id="LID909" rel="#L909">909</span>
<span id="LID910" rel="#L910">910</span>
<span id="LID911" rel="#L911">911</span>
<span id="LID912" rel="#L912">912</span>
<span id="LID913" rel="#L913">913</span>
<span id="LID914" rel="#L914">914</span>
<span id="LID915" rel="#L915">915</span>
<span id="LID916" rel="#L916">916</span>
<span id="LID917" rel="#L917">917</span>
<span id="LID918" rel="#L918">918</span>
<span id="LID919" rel="#L919">919</span>
<span id="LID920" rel="#L920">920</span>
<span id="LID921" rel="#L921">921</span>
<span id="LID922" rel="#L922">922</span>
<span id="LID923" rel="#L923">923</span>
<span id="LID924" rel="#L924">924</span>
<span id="LID925" rel="#L925">925</span>
<span id="LID926" rel="#L926">926</span>
<span id="LID927" rel="#L927">927</span>
<span id="LID928" rel="#L928">928</span>
<span id="LID929" rel="#L929">929</span>
<span id="LID930" rel="#L930">930</span>
<span id="LID931" rel="#L931">931</span>
<span id="LID932" rel="#L932">932</span>
<span id="LID933" rel="#L933">933</span>
<span id="LID934" rel="#L934">934</span>
<span id="LID935" rel="#L935">935</span>
<span id="LID936" rel="#L936">936</span>
<span id="LID937" rel="#L937">937</span>
<span id="LID938" rel="#L938">938</span>
<span id="LID939" rel="#L939">939</span>
<span id="LID940" rel="#L940">940</span>
<span id="LID941" rel="#L941">941</span>
<span id="LID942" rel="#L942">942</span>
<span id="LID943" rel="#L943">943</span>
<span id="LID944" rel="#L944">944</span>
<span id="LID945" rel="#L945">945</span>
<span id="LID946" rel="#L946">946</span>
<span id="LID947" rel="#L947">947</span>
<span id="LID948" rel="#L948">948</span>
<span id="LID949" rel="#L949">949</span>
<span id="LID950" rel="#L950">950</span>
<span id="LID951" rel="#L951">951</span>
<span id="LID952" rel="#L952">952</span>
<span id="LID953" rel="#L953">953</span>
<span id="LID954" rel="#L954">954</span>
<span id="LID955" rel="#L955">955</span>
<span id="LID956" rel="#L956">956</span>
<span id="LID957" rel="#L957">957</span>
</pre>
          </td>
          <td width="100%">
            
              <div class="highlight"><pre><div class="line" id="LC1"><span class="c1">;;;; ESRAP -- a packrat parser for Common Lisp</span></div><div class="line" id="LC2"><span class="c1">;;;; by Nikodemus Siivola, 2007</span></div><div class="line" id="LC3"><span class="c1">;;;;</span></div><div class="line" id="LC4"><span class="c1">;;;; In addition to regular Packrat / Parsing Grammar / TDPL features</span></div><div class="line" id="LC5"><span class="c1">;;;; ESRAP supports:</span></div><div class="line" id="LC6"><span class="c1">;;;;</span></div><div class="line" id="LC7"><span class="c1">;;;;  - dynamic redefinition of nonterminals</span></div><div class="line" id="LC8"><span class="c1">;;;;  - inline grammars</span></div><div class="line" id="LC9"><span class="c1">;;;;  - semantic predicates</span></div><div class="line" id="LC10"><span class="c1">;;;;  - introspective facilities</span></div><div class="line" id="LC11"><span class="c1">;;;;</span></div><div class="line" id="LC12"><span class="c1">;;;; References:</span></div><div class="line" id="LC13"><span class="c1">;;;;</span></div><div class="line" id="LC14"><span class="c1">;;;;   * Bryan Ford, 2002, &quot;Packrat Parsing: a Practical Linear Time</span></div><div class="line" id="LC15"><span class="c1">;;;;     Algorithm with Backtracking&quot;.</span></div><div class="line" id="LC16"><span class="c1">;;;;     http://pdos.csail.mit.edu/~baford/packrat/thesis/</span></div><div class="line" id="LC17"><span class="c1">;;;;</span></div><div class="line" id="LC18"><span class="c1">;;;; Licence:</span></div><div class="line" id="LC19"><span class="c1">;;;;</span></div><div class="line" id="LC20"><span class="c1">;;;;  Permission is hereby granted, free of charge, to any person</span></div><div class="line" id="LC21"><span class="c1">;;;;  obtaining a copy of this software and associated documentation files</span></div><div class="line" id="LC22"><span class="c1">;;;;  (the &quot;Software&quot;), to deal in the Software without restriction,</span></div><div class="line" id="LC23"><span class="c1">;;;;  including without limitation the rights to use, copy, modify, merge,</span></div><div class="line" id="LC24"><span class="c1">;;;;  publish, distribute, sublicense, and/or sell copies of the Software,</span></div><div class="line" id="LC25"><span class="c1">;;;;  and to permit persons to whom the Software is furnished to do so,</span></div><div class="line" id="LC26"><span class="c1">;;;;  subject to the following conditions:</span></div><div class="line" id="LC27"><span class="c1">;;;;</span></div><div class="line" id="LC28"><span class="c1">;;;;  THE SOFTWARE IS PROVIDED &quot;AS IS&quot;, WITHOUT WARRANTY OF ANY KIND,</span></div><div class="line" id="LC29"><span class="c1">;;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF</span></div><div class="line" id="LC30"><span class="c1">;;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.</span></div><div class="line" id="LC31"><span class="c1">;;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY</span></div><div class="line" id="LC32"><span class="c1">;;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,</span></div><div class="line" id="LC33"><span class="c1">;;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE</span></div><div class="line" id="LC34"><span class="c1">;;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.</span></div><div class="line" id="LC35"><span class="c1">;;;;</span></div><div class="line" id="LC36"><span class="c1">;;;; Syntax overview:</span></div><div class="line" id="LC37"><span class="c1">;;;;</span></div><div class="line" id="LC38"><span class="c1">;;;;  character                 -- any single character</span></div><div class="line" id="LC39"><span class="c1">;;;;  (string length)           -- any string of length</span></div><div class="line" id="LC40"><span class="c1">;;;;  (and &amp;rest sequence)</span></div><div class="line" id="LC41"><span class="c1">;;;;  (or &amp;rest ordered-choises)</span></div><div class="line" id="LC42"><span class="c1">;;;;  (* greedy-repetition)</span></div><div class="line" id="LC43"><span class="c1">;;;;  (+ greedy-positive-repetition)</span></div><div class="line" id="LC44"><span class="c1">;;;;  (? optional)</span></div><div class="line" id="LC45"><span class="c1">;;;;  (&amp; followed-by)           -- does not consume</span></div><div class="line" id="LC46"><span class="c1">;;;;  (! not-followed-by)       -- does not consume</span></div><div class="line" id="LC47"><span class="c1">;;;;  (&lt;predicate&gt; expr)        -- semantic parsing</span></div><div class="line" id="LC48"><span class="c1">;;;;</span></div><div class="line" id="LC49"><span class="c1">;;;; Examples:</span></div><div class="line" id="LC50"><span class="c1">;;;;</span></div><div class="line" id="LC51"><span class="c1">;;;;  (parse &#39;(or &quot;foo&quot; &quot;bar&quot;) &quot;foo&quot;)         =&gt; &quot;foo&quot;, NIL</span></div><div class="line" id="LC52"><span class="c1">;;;;</span></div><div class="line" id="LC53"><span class="c1">;;;;  (add-rule &#39;foo+ (make-instance &#39;rule :expression &#39;(+ &quot;foo&quot;))) =&gt; FOO+</span></div><div class="line" id="LC54"><span class="c1">;;;;</span></div><div class="line" id="LC55"><span class="c1">;;;;  (parse &#39;foo+ &quot;foofoofoo&quot;)               =&gt; (&quot;foo&quot; &quot;foo&quot; &quot;foo&quot;), NIL</span></div><div class="line" id="LC56"><span class="c1">;;;;</span></div><div class="line" id="LC57"><span class="c1">;;;;  (add-rule &#39;decimal</span></div><div class="line" id="LC58"><span class="c1">;;;;            (make-instance &#39;rule</span></div><div class="line" id="LC59"><span class="c1">;;;;             :expression &#39;(+ (or &quot;0&quot; &quot;1&quot; &quot;2&quot; &quot;3&quot; &quot;4&quot; &quot;5&quot; &quot;6&quot; &quot;7&quot; &quot;8&quot; &quot;9&quot;))</span></div><div class="line" id="LC60"><span class="c1">;;;;             :transform (lambda (list) </span></div><div class="line" id="LC61"><span class="c1">;;;;                          (parse-integer (format nil &quot;~{~A~}&quot; list)))))</span></div><div class="line" id="LC62"><span class="c1">;;;;   =&gt; DECIMAL</span></div><div class="line" id="LC63"><span class="c1">;;;;</span></div><div class="line" id="LC64"><span class="c1">;;;;  (parse &#39;(oddp decimal) &quot;123&quot;)                  =&gt; 123</span></div><div class="line" id="LC65"><span class="c1">;;;;</span></div><div class="line" id="LC66"><span class="c1">;;;;  (parse &#39;(evenp decimal) &quot;123&quot; :junk-allowed t) =&gt; NIL, 0</span></div><div class="line" id="LC67"><span class="c1">;;;;</span></div><div class="line" id="LC68"><span class="c1">;;;; TODO:</span></div><div class="line" id="LC69"><span class="c1">;;;;  - character classes</span></div><div class="line" id="LC70"><span class="c1">;;;;  - nicer error messages</span></div><div class="line" id="LC71"><span class="c1">;;;;  - setting breaks on rules</span></div><div class="line" id="LC72"><span class="c1">;;;;  - proper tests</span></div><div class="line" id="LC73"><span class="c1">;;;;  - states</span></div><div class="line" id="LC74"><span class="c1">;;;;  - documentation</span></div><div class="line" id="LC75"><span class="c1">;;;;  - transform-production vs. transform-subseq:</span></div><div class="line" id="LC76"><span class="c1">;;;;    (add-rule &#39;decimal :expression &#39;(+ (or &quot;0&quot; &quot;1&quot; ...))</span></div><div class="line" id="LC77"><span class="c1">;;;;                       :transform-subseq #&#39;parse-integer)</span></div><div class="line" id="LC78"><span class="c1">;;;;  - optimizing single-character alternatives: store in a string,</span></div><div class="line" id="LC79"><span class="c1">;;;;    not in a list.</span></div><div class="line" id="LC80">&nbsp;</div><div class="line" id="LC81"><span class="p">(</span><span class="k">eval-when</span> <span class="p">(</span><span class="ss">:compile-toplevel</span> <span class="ss">:load-toplevel</span> <span class="ss">:execute</span><span class="p">)</span></div><div class="line" id="LC82">&nbsp;&nbsp;</div><div class="line" id="LC83">&nbsp;&nbsp;<span class="p">(</span><span class="nb">require</span> <span class="ss">:alexandria</span><span class="p">)</span></div><div class="line" id="LC84">&nbsp;&nbsp;</div><div class="line" id="LC85">&nbsp;&nbsp;<span class="p">(</span><span class="nb">defpackage</span> <span class="ss">:esrap</span></div><div class="line" id="LC86">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="ss">:use</span> <span class="ss">:cl</span> <span class="ss">:alexandria</span><span class="p">)</span></div><div class="line" id="LC87">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="ss">:export</span> </div><div class="line" id="LC88">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">#:!</span> <span class="ss">#:?</span> <span class="ss">#:+</span> <span class="ss">#:*</span></div><div class="line" id="LC89">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">#:add-rule</span></div><div class="line" id="LC90">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">#:concat</span></div><div class="line" id="LC91">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">#:describe-grammar</span></div><div class="line" id="LC92">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">#:defrule</span></div><div class="line" id="LC93">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">#:esrap-version</span></div><div class="line" id="LC94">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">#:find-rule</span></div><div class="line" id="LC95">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">#:parse</span> </div><div class="line" id="LC96">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">#:rule</span></div><div class="line" id="LC97">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">#:rule-dependencies</span></div><div class="line" id="LC98">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">#:remove-rule</span></div><div class="line" id="LC99">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">)))</span></div><div class="line" id="LC100">&nbsp;</div><div class="line" id="LC101"><span class="p">(</span><span class="nb">in-package</span> <span class="ss">:esrap</span><span class="p">)</span></div><div class="line" id="LC102">&nbsp;</div><div class="line" id="LC103"><span class="c1">;;; Miscellany</span></div><div class="line" id="LC104">&nbsp;</div><div class="line" id="LC105"><span class="p">(</span><span class="nb">defun</span> <span class="nv">esrap-version</span> <span class="p">()</span></div><div class="line" id="LC106">&nbsp;&nbsp;<span class="s">&quot;0.1&quot;</span><span class="p">)</span></div><div class="line" id="LC107">&nbsp;</div><div class="line" id="LC108"><span class="p">(</span><span class="nb">defun</span> <span class="nv">concat</span> <span class="p">(</span><span class="k">&amp;rest</span> <span class="nv">arguments</span><span class="p">)</span></div><div class="line" id="LC109">&nbsp;&nbsp;<span class="s">&quot;Arguments must be strings, or lists whose leaves are strings.</span></div><div class="line" id="LC110"><span class="s">Catenates all the strings in arguments into a single string.&quot;</span></div><div class="line" id="LC111">&nbsp;&nbsp;<span class="p">(</span><span class="nb">with-output-to-string</span> <span class="p">(</span><span class="nv">s</span><span class="p">)</span></div><div class="line" id="LC112">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">labels</span> <span class="p">((</span><span class="nv">cat-list</span> <span class="p">(</span><span class="nb">list</span><span class="p">)</span></div><div class="line" id="LC113">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dolist</span> <span class="p">(</span><span class="nb">elt</span> <span class="nb">list</span><span class="p">)</span></div><div class="line" id="LC114">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">etypecase</span> <span class="nb">elt</span></div><div class="line" id="LC115">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">string</span> <span class="p">(</span><span class="nb">write-string</span> <span class="nb">elt</span> <span class="nv">s</span><span class="p">))</span></div><div class="line" id="LC116">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">list</span> <span class="p">(</span><span class="nv">cat-list</span> <span class="nb">elt</span><span class="p">))))))</span></div><div class="line" id="LC117">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">cat-list</span> <span class="nv">arguments</span><span class="p">))))</span></div><div class="line" id="LC118">&nbsp;</div><div class="line" id="LC119"><span class="p">(</span><span class="nb">deftype</span> <span class="nv">nonterminal</span> <span class="p">()</span></div><div class="line" id="LC120">&nbsp;&nbsp;<span class="s">&quot;Any symbol except CHARACTER and NIL can be used as a nonterminal symbol.&quot;</span></div><div class="line" id="LC121">&nbsp;&nbsp;<span class="o">&#39;</span><span class="p">(</span><span class="nb">and</span> <span class="nc">symbol</span> <span class="p">(</span><span class="nb">not</span> <span class="p">(</span><span class="nb">member</span> <span class="nb">character</span> <span class="no">nil</span><span class="p">))))</span></div><div class="line" id="LC122">&nbsp;</div><div class="line" id="LC123"><span class="p">(</span><span class="nb">deftype</span> <span class="nv">terminal</span> <span class="p">()</span></div><div class="line" id="LC124">&nbsp;&nbsp;<span class="s">&quot;Strings and characters are used as terminal symbols.&quot;</span></div><div class="line" id="LC125">&nbsp;&nbsp;<span class="o">&#39;</span><span class="p">(</span><span class="nb">or</span> <span class="nb">string</span> <span class="nb">character</span><span class="p">))</span></div><div class="line" id="LC126">&nbsp;</div><div class="line" id="LC127"><span class="c1">;;; RULE REPRESENTATION AND STORAGE</span></div><div class="line" id="LC128"><span class="c1">;;;</span></div><div class="line" id="LC129"><span class="c1">;;; For each rule, there is a cons cell in *RULES*, which has the</span></div><div class="line" id="LC130"><span class="c1">;;; function that implements the rule in car, and the rule object</span></div><div class="line" id="LC131"><span class="c1">;;; in CDR. A RULE object can be attaches to only one non-terminal</span></div><div class="line" id="LC132"><span class="c1">;;; at a time, which is accessible via RULE-SYMBOL.</span></div><div class="line" id="LC133">&nbsp;</div><div class="line" id="LC134"><span class="p">(</span><span class="nb">defvar</span> <span class="vg">*rules*</span> <span class="p">(</span><span class="nb">make-hash-table</span><span class="p">))</span></div><div class="line" id="LC135">&nbsp;</div><div class="line" id="LC136"><span class="p">(</span><span class="nb">defun</span> <span class="nv">clear-rules</span> <span class="p">()</span></div><div class="line" id="LC137">&nbsp;&nbsp;<span class="p">(</span><span class="nb">clrhash</span> <span class="vg">*rules*</span><span class="p">)</span></div><div class="line" id="LC138">&nbsp;&nbsp;<span class="no">nil</span><span class="p">)</span></div><div class="line" id="LC139">&nbsp;</div><div class="line" id="LC140"><span class="p">(</span><span class="nb">deftype</span> <span class="nv">rule-cell</span> <span class="p">()</span></div><div class="line" id="LC141">&nbsp;&nbsp;<span class="o">&#39;</span><span class="p">(</span><span class="nb">cons</span> <span class="k">function</span> <span class="p">(</span><span class="nb">cons</span> <span class="no">t</span> <span class="no">t</span><span class="p">)))</span></div><div class="line" id="LC142">&nbsp;</div><div class="line" id="LC143"><span class="p">(</span><span class="nb">declaim</span> <span class="p">(</span><span class="k">inline</span> <span class="nv">cell-function</span><span class="p">))</span></div><div class="line" id="LC144"><span class="p">(</span><span class="nb">defun</span> <span class="nv">cell-function</span> <span class="p">(</span><span class="nv">cell</span><span class="p">)</span> </div><div class="line" id="LC145">&nbsp;&nbsp;<span class="p">(</span><span class="nb">car</span> <span class="nv">cell</span><span class="p">))</span></div><div class="line" id="LC146">&nbsp;</div><div class="line" id="LC147"><span class="p">(</span><span class="nb">defun</span> <span class="p">(</span><span class="nb">setf</span> <span class="nv">cell-function</span><span class="p">)</span> <span class="p">(</span><span class="k">function</span> <span class="nv">cell</span><span class="p">)</span></div><div class="line" id="LC148">&nbsp;&nbsp;<span class="p">(</span><span class="k">declare</span> <span class="p">(</span><span class="k">function</span> <span class="k">function</span><span class="p">)</span> <span class="p">(</span><span class="nv">rule-cell</span> <span class="nv">cell</span><span class="p">))</span></div><div class="line" id="LC149">&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">cell</span><span class="p">)</span> <span class="k">function</span><span class="p">))</span></div><div class="line" id="LC150">&nbsp;</div><div class="line" id="LC151"><span class="p">(</span><span class="nb">defun</span> <span class="nv">cell-referents</span> <span class="p">(</span><span class="nv">cell</span><span class="p">)</span></div><div class="line" id="LC152">&nbsp;&nbsp;<span class="p">(</span><span class="nb">car</span> <span class="p">(</span><span class="nb">cdr</span> <span class="nv">cell</span><span class="p">)))</span></div><div class="line" id="LC153">&nbsp;</div><div class="line" id="LC154"><span class="p">(</span><span class="nb">defun</span> <span class="p">(</span><span class="nb">setf</span> <span class="nv">cell-referents</span><span class="p">)</span> <span class="p">(</span><span class="nb">list</span> <span class="nv">cell</span><span class="p">)</span></div><div class="line" id="LC155">&nbsp;&nbsp;<span class="p">(</span><span class="k">declare</span> <span class="p">(</span><span class="nv">rule-cell</span> <span class="nv">cell</span><span class="p">))</span></div><div class="line" id="LC156">&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nb">car</span> <span class="p">(</span><span class="nb">cdr</span> <span class="nv">cell</span><span class="p">))</span> <span class="nb">list</span><span class="p">))</span></div><div class="line" id="LC157">&nbsp;</div><div class="line" id="LC158"><span class="p">(</span><span class="nb">defun</span> <span class="nv">cell-rule</span> <span class="p">(</span><span class="nv">cell</span><span class="p">)</span></div><div class="line" id="LC159">&nbsp;&nbsp;<span class="p">(</span><span class="nb">cdr</span> <span class="p">(</span><span class="nb">cdr</span> <span class="nv">cell</span><span class="p">)))</span></div><div class="line" id="LC160">&nbsp;</div><div class="line" id="LC161"><span class="p">(</span><span class="nb">defun</span> <span class="p">(</span><span class="nb">setf</span> <span class="nv">cell-rule</span><span class="p">)</span> <span class="p">(</span><span class="nv">rule</span> <span class="nv">cell</span><span class="p">)</span></div><div class="line" id="LC162">&nbsp;&nbsp;<span class="p">(</span><span class="k">declare</span> <span class="p">(</span><span class="nv">rule-cell</span> <span class="nv">cell</span><span class="p">))</span></div><div class="line" id="LC163">&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nb">cdr</span> <span class="p">(</span><span class="nb">cdr</span> <span class="nv">cell</span><span class="p">))</span> <span class="nv">rule</span><span class="p">))</span></div><div class="line" id="LC164">&nbsp;</div><div class="line" id="LC165"><span class="p">(</span><span class="nb">defun</span> <span class="nv">ensure-rule-cell</span> <span class="p">(</span><span class="nc">symbol</span><span class="p">)</span></div><div class="line" id="LC166">&nbsp;&nbsp;<span class="p">(</span><span class="nb">check-type</span> <span class="nc">symbol</span> <span class="nv">nonterminal</span><span class="p">)</span></div><div class="line" id="LC167">&nbsp;&nbsp;<span class="c1">;; FIXME: Need to lock *RULES*.</span></div><div class="line" id="LC168">&nbsp;&nbsp;<span class="p">(</span><span class="nb">or</span> <span class="p">(</span><span class="nb">gethash</span> <span class="nc">symbol</span> <span class="vg">*rules*</span><span class="p">)</span></div><div class="line" id="LC169">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nb">gethash</span> <span class="nc">symbol</span> <span class="vg">*rules*</span><span class="p">)</span></div><div class="line" id="LC170">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cons</span> <span class="p">(</span><span class="k">lambda</span> <span class="p">(</span><span class="k">&amp;rest</span> <span class="nv">args</span><span class="p">)</span></div><div class="line" id="LC171">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">declare</span> <span class="p">(</span><span class="k">ignore</span> <span class="nv">args</span><span class="p">))</span></div><div class="line" id="LC172">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">error</span> <span class="s">&quot;Undefined rule: ~S&quot;</span> <span class="nc">symbol</span><span class="p">))</span></div><div class="line" id="LC173">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cons</span> <span class="no">nil</span> <span class="no">nil</span><span class="p">)))))</span></div><div class="line" id="LC174">&nbsp;</div><div class="line" id="LC175"><span class="p">(</span><span class="nb">defun</span> <span class="nv">delete-rule-cell</span> <span class="p">(</span><span class="nc">symbol</span><span class="p">)</span></div><div class="line" id="LC176">&nbsp;&nbsp;<span class="p">(</span><span class="nb">remhash</span> <span class="nc">symbol</span> <span class="vg">*rules*</span><span class="p">))</span></div><div class="line" id="LC177">&nbsp;</div><div class="line" id="LC178"><span class="p">(</span><span class="nb">defun</span> <span class="nv">reference-rule-cell</span> <span class="p">(</span><span class="nc">symbol</span> <span class="nv">referent</span><span class="p">)</span></div><div class="line" id="LC179">&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">cell</span> <span class="p">(</span><span class="nv">ensure-rule-cell</span> <span class="nc">symbol</span><span class="p">)))</span></div><div class="line" id="LC180">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">pushnew</span> <span class="nv">referent</span> <span class="p">(</span><span class="nv">cell-referents</span> <span class="nv">cell</span><span class="p">))</span></div><div class="line" id="LC181">&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">cell</span><span class="p">))</span></div><div class="line" id="LC182">&nbsp;</div><div class="line" id="LC183"><span class="p">(</span><span class="nb">defun</span> <span class="nv">dereference-rule-cell</span> <span class="p">(</span><span class="nc">symbol</span> <span class="nv">referent</span><span class="p">)</span></div><div class="line" id="LC184">&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">cell</span> <span class="p">(</span><span class="nv">ensure-rule-cell</span> <span class="nc">symbol</span><span class="p">)))</span></div><div class="line" id="LC185">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nv">cell-referents</span> <span class="nv">cell</span><span class="p">)</span> <span class="p">(</span><span class="nb">delete</span> <span class="nv">referent</span> <span class="p">(</span><span class="nv">cell-referents</span> <span class="nv">cell</span><span class="p">)))</span></div><div class="line" id="LC186">&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">cell</span><span class="p">))</span></div><div class="line" id="LC187">&nbsp;</div><div class="line" id="LC188"><span class="p">(</span><span class="nb">defun</span> <span class="nv">find-rule-cell</span> <span class="p">(</span><span class="nc">symbol</span><span class="p">)</span></div><div class="line" id="LC189">&nbsp;&nbsp;<span class="p">(</span><span class="nb">check-type</span> <span class="nc">symbol</span> <span class="nv">nonterminal</span><span class="p">)</span></div><div class="line" id="LC190">&nbsp;&nbsp;<span class="p">(</span><span class="nb">gethash</span> <span class="nc">symbol</span> <span class="vg">*rules*</span><span class="p">))</span></div><div class="line" id="LC191">&nbsp;</div><div class="line" id="LC192"><span class="p">(</span><span class="nb">defclass</span> <span class="nv">rule</span> <span class="p">()</span></div><div class="line" id="LC193">&nbsp;&nbsp;<span class="p">((</span><span class="nv">%symbol</span></div><div class="line" id="LC194">&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:initform</span> <span class="no">nil</span></div><div class="line" id="LC195">&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:reader</span> <span class="nv">rule-symbol</span><span class="p">)</span></div><div class="line" id="LC196">&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">%expression</span> </div><div class="line" id="LC197">&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:initarg</span> <span class="ss">:expression</span></div><div class="line" id="LC198">&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:initform</span> <span class="p">(</span><span class="nv">required-argument</span> <span class="ss">:expression</span><span class="p">)</span></div><div class="line" id="LC199">&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:reader</span> <span class="nv">rule-expression</span><span class="p">)</span></div><div class="line" id="LC200">&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">%transform</span></div><div class="line" id="LC201">&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:initarg</span> <span class="ss">:transform</span></div><div class="line" id="LC202">&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:initform</span> <span class="no">nil</span></div><div class="line" id="LC203">&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:reader</span> <span class="nv">rule-transform</span><span class="p">)))</span></div><div class="line" id="LC204">&nbsp;</div><div class="line" id="LC205"><span class="p">(</span><span class="nb">defun</span> <span class="nv">detach-rule</span> <span class="p">(</span><span class="nv">rule</span><span class="p">)</span></div><div class="line" id="LC206">&nbsp;&nbsp;<span class="p">(</span><span class="nb">dolist</span> <span class="p">(</span><span class="nv">dep</span> <span class="p">(</span><span class="nv">rule-direct-dependencies</span> <span class="nv">rule</span><span class="p">))</span></div><div class="line" id="LC207">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">dereference-rule-cell</span> <span class="nv">dep</span> <span class="p">(</span><span class="nv">rule-symbol</span> <span class="nv">rule</span><span class="p">)))</span></div><div class="line" id="LC208">&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nb">slot-value</span> <span class="nv">rule</span> <span class="ss">&#39;%symbol</span><span class="p">)</span> <span class="no">nil</span><span class="p">))</span></div><div class="line" id="LC209">&nbsp;</div><div class="line" id="LC210"><span class="p">(</span><span class="nb">defmethod</span> <span class="nb">shared-initialize</span> <span class="ss">:after</span> <span class="p">((</span><span class="nv">rule</span> <span class="nv">rule</span><span class="p">)</span> <span class="nv">slots</span> <span class="k">&amp;key</span><span class="p">)</span></div><div class="line" id="LC211">&nbsp;&nbsp;<span class="p">(</span><span class="nv">validate-expression</span> <span class="p">(</span><span class="nv">rule-expression</span> <span class="nv">rule</span><span class="p">)))</span></div><div class="line" id="LC212">&nbsp;</div><div class="line" id="LC213"><span class="p">(</span><span class="nb">defmethod</span> <span class="nb">print-object</span> <span class="p">((</span><span class="nv">rule</span> <span class="nv">rule</span><span class="p">)</span> <span class="nc">stream</span><span class="p">)</span></div><div class="line" id="LC214">&nbsp;&nbsp;<span class="p">(</span><span class="nb">print-unreadable-object</span> <span class="p">(</span><span class="nv">rule</span> <span class="nc">stream</span> <span class="ss">:type</span> <span class="no">t</span> <span class="ss">:identity</span> <span class="no">nil</span><span class="p">)</span></div><div class="line" id="LC215">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nc">symbol</span> <span class="p">(</span><span class="nv">rule-symbol</span> <span class="nv">rule</span><span class="p">)))</span></div><div class="line" id="LC216">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="nc">symbol</span></div><div class="line" id="LC217">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">format</span> <span class="nc">stream</span> <span class="s">&quot;~S &lt;- &quot;</span> <span class="nc">symbol</span><span class="p">)</span></div><div class="line" id="LC218">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">format</span> <span class="nc">stream</span> <span class="s">&quot;(detached) &quot;</span><span class="p">)))</span></div><div class="line" id="LC219">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">write</span> <span class="p">(</span><span class="nv">rule-expression</span> <span class="nv">rule</span><span class="p">)</span> <span class="ss">:stream</span> <span class="nc">stream</span><span class="p">)))</span></div><div class="line" id="LC220">&nbsp;</div><div class="line" id="LC221"><span class="p">(</span><span class="nb">defun</span> <span class="nv">sort-dependencies</span> <span class="p">(</span><span class="nc">symbol</span> <span class="nv">dependencies</span><span class="p">)</span></div><div class="line" id="LC222">&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">symbols</span> <span class="p">(</span><span class="nb">delete</span> <span class="nc">symbol</span> <span class="nv">dependencies</span><span class="p">))</span></div><div class="line" id="LC223">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">defined</span> <span class="no">nil</span><span class="p">)</span></div><div class="line" id="LC224">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">undefined</span> <span class="no">nil</span><span class="p">))</span></div><div class="line" id="LC225">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dolist</span> <span class="p">(</span><span class="nv">sym</span> <span class="nv">symbols</span><span class="p">)</span></div><div class="line" id="LC226">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">find-rule</span> <span class="nv">sym</span><span class="p">)</span></div><div class="line" id="LC227">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">push</span> <span class="nv">sym</span> <span class="nv">defined</span><span class="p">)</span></div><div class="line" id="LC228">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">push</span> <span class="nv">sym</span> <span class="nv">undefined</span><span class="p">)))</span></div><div class="line" id="LC229">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">values</span> <span class="nv">defined</span> <span class="nv">undefined</span><span class="p">)))</span></div><div class="line" id="LC230">&nbsp;</div><div class="line" id="LC231"><span class="p">(</span><span class="nb">defun</span> <span class="nv">rule-dependencies</span> <span class="p">(</span><span class="nv">rule</span><span class="p">)</span></div><div class="line" id="LC232">&nbsp;&nbsp;<span class="s">&quot;Returns the dependencies of the RULE: primary value is a list of defined</span></div><div class="line" id="LC233"><span class="s">nonterminal symbols, and secondary value is a list of undefined nonterminal</span></div><div class="line" id="LC234"><span class="s">symbols.&quot;</span></div><div class="line" id="LC235">&nbsp;&nbsp;<span class="p">(</span><span class="nv">sort-dependencies</span> </div><div class="line" id="LC236">&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">rule-symbol</span> <span class="nv">rule</span><span class="p">)</span> <span class="p">(</span><span class="nv">%expression-dependencies</span> <span class="p">(</span><span class="nv">rule-expression</span> <span class="nv">rule</span><span class="p">)</span> <span class="no">nil</span><span class="p">)))</span></div><div class="line" id="LC237">&nbsp;</div><div class="line" id="LC238"><span class="p">(</span><span class="nb">defun</span> <span class="nv">rule-direct-dependencies</span> <span class="p">(</span><span class="nv">rule</span><span class="p">)</span></div><div class="line" id="LC239">&nbsp;&nbsp;<span class="p">(</span><span class="nv">sort-dependencies</span></div><div class="line" id="LC240">&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">rule-symbol</span> <span class="nv">rule</span><span class="p">)</span> <span class="p">(</span><span class="nv">%expression-direct-dependencies</span> <span class="p">(</span><span class="nv">rule-expression</span> <span class="nv">rule</span><span class="p">)</span> <span class="no">nil</span><span class="p">)))</span></div><div class="line" id="LC241">&nbsp;</div><div class="line" id="LC242"><span class="c1">;;; Expression destructuring and validation</span></div><div class="line" id="LC243">&nbsp;</div><div class="line" id="LC244"><span class="p">(</span><span class="nb">defmacro</span> <span class="nv">with-expression</span> <span class="p">((</span><span class="nv">expr</span> <span class="nv">lambda-list</span><span class="p">)</span> <span class="k">&amp;body</span> <span class="nv">body</span><span class="p">)</span></div><div class="line" id="LC245">&nbsp;&nbsp;<span class="p">(</span><span class="k">let*</span> <span class="p">((</span><span class="k">type</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">lambda-list</span><span class="p">))</span></div><div class="line" id="LC246">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">car-var</span> <span class="p">(</span><span class="nb">gensym</span> <span class="s">&quot;CAR&quot;</span><span class="p">))</span></div><div class="line" id="LC247">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">fixed-list</span> <span class="p">(</span><span class="nb">cons</span> <span class="nv">car-var</span> <span class="p">(</span><span class="nb">cdr</span> <span class="nv">lambda-list</span><span class="p">))))</span></div><div class="line" id="LC248">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">once-only</span> <span class="p">(</span><span class="nv">expr</span><span class="p">)</span></div><div class="line" id="LC249">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">`</span><span class="p">(</span><span class="nb">destructuring-bind</span> <span class="o">,</span><span class="nv">fixed-list</span> <span class="o">,</span><span class="nv">expr</span></div><div class="line" id="LC250">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">,</span><span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">eq</span> <span class="no">t</span> <span class="k">type</span><span class="p">)</span></div><div class="line" id="LC251">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">`</span><span class="p">(</span><span class="k">declare</span> <span class="p">(</span><span class="k">ignore</span> <span class="o">,</span><span class="nv">car-var</span><span class="p">))</span></div><div class="line" id="LC252">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">`</span><span class="p">(</span><span class="nb">unless</span> <span class="p">(</span><span class="nb">eq</span> <span class="ss">&#39;,type</span> <span class="o">,</span><span class="nv">car-var</span><span class="p">)</span></div><div class="line" id="LC253">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">error</span> <span class="s">&quot;~S-expression expected, got: ~S&quot;</span> <span class="ss">&#39;,type</span> <span class="o">,</span><span class="nv">expr</span><span class="p">)))</span></div><div class="line" id="LC254">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">locally</span> <span class="o">,@</span><span class="nv">body</span><span class="p">)))))</span></div><div class="line" id="LC255">&nbsp;</div><div class="line" id="LC256"><span class="c1">;;; MEMOIZATION CACHE</span></div><div class="line" id="LC257"><span class="c1">;;;</span></div><div class="line" id="LC258"><span class="c1">;;; Because each [rule, position] tuple has an unambiguous</span></div><div class="line" id="LC259"><span class="c1">;;; result per source text, we can cache this result -- this is what</span></div><div class="line" id="LC260"><span class="c1">;;; makes packrat parsing O(N).</span></div><div class="line" id="LC261"><span class="c1">;;;</span></div><div class="line" id="LC262"><span class="c1">;;; For now we just use EQUAL hash-tables, but a specialized</span></div><div class="line" id="LC263"><span class="c1">;;; representation would probably pay off.</span></div><div class="line" id="LC264">&nbsp;</div><div class="line" id="LC265"><span class="p">(</span><span class="nb">defvar</span> <span class="vg">*cache*</span><span class="p">)</span></div><div class="line" id="LC266">&nbsp;</div><div class="line" id="LC267"><span class="p">(</span><span class="nb">defun</span> <span class="nv">make-cache</span> <span class="p">()</span></div><div class="line" id="LC268">&nbsp;&nbsp;<span class="p">(</span><span class="nb">make-hash-table</span> <span class="ss">:test</span> <span class="nf">#&#39;</span><span class="nb">equal</span><span class="p">))</span></div><div class="line" id="LC269">&nbsp;</div><div class="line" id="LC270"><span class="p">(</span><span class="nb">defun</span> <span class="nv">get-cached</span> <span class="p">(</span><span class="nc">symbol</span> <span class="nb">position</span> <span class="nv">cache</span><span class="p">)</span></div><div class="line" id="LC271">&nbsp;&nbsp;<span class="p">(</span><span class="nb">gethash</span> <span class="p">(</span><span class="nb">cons</span> <span class="nc">symbol</span> <span class="nb">position</span><span class="p">)</span> <span class="nv">cache</span><span class="p">))</span></div><div class="line" id="LC272">&nbsp;</div><div class="line" id="LC273"><span class="p">(</span><span class="nb">defun</span> <span class="p">(</span><span class="nb">setf</span> <span class="nv">get-cached</span><span class="p">)</span> <span class="p">(</span><span class="nv">result</span> <span class="nc">symbol</span> <span class="nb">position</span> <span class="nv">cache</span><span class="p">)</span></div><div class="line" id="LC274">&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nb">gethash</span> <span class="p">(</span><span class="nb">cons</span> <span class="nc">symbol</span> <span class="nb">position</span><span class="p">)</span> <span class="nv">cache</span><span class="p">)</span> <span class="nv">result</span><span class="p">))</span></div><div class="line" id="LC275">&nbsp;</div><div class="line" id="LC276"><span class="p">(</span><span class="nb">defvar</span> <span class="vg">*nonterminal-stack*</span> <span class="no">nil</span><span class="p">)</span></div><div class="line" id="LC277">&nbsp;</div><div class="line" id="LC278"><span class="c1">;;; SYMBOL, POSITION, and CACHE must all be lexical variables!</span></div><div class="line" id="LC279"><span class="p">(</span><span class="nb">defmacro</span> <span class="nv">with-cached-result</span> <span class="p">((</span><span class="nc">symbol</span> <span class="nb">position</span><span class="p">)</span> <span class="k">&amp;body</span> <span class="nv">forms</span><span class="p">)</span></div><div class="line" id="LC280">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-gensyms</span> <span class="p">(</span><span class="nv">cache</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC281">&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">`</span><span class="p">(</span><span class="k">let*</span> <span class="p">((</span><span class="o">,</span><span class="nv">cache</span> <span class="vg">*cache*</span><span class="p">)</span></div><div class="line" id="LC282">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="o">,</span><span class="nv">result</span> <span class="p">(</span><span class="nv">get-cached</span> <span class="o">,</span><span class="nc">symbol</span> <span class="o">,</span><span class="nb">position</span> <span class="o">,</span><span class="nv">cache</span><span class="p">))</span></div><div class="line" id="LC283">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="vg">*nonterminal-stack*</span> <span class="p">(</span><span class="nb">cons</span> <span class="o">,</span><span class="nc">symbol</span> <span class="vg">*nonterminal-stack*</span><span class="p">)))</span></div><div class="line" id="LC284">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cond</span> <span class="p">((</span><span class="nb">eq</span> <span class="no">t</span> <span class="o">,</span><span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC285">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">error</span> <span class="s">&quot;Left recursion in nonterminal ~S, at ~S.~%Path: ~{~S~^ -&gt; ~}&quot;</span> </div><div class="line" id="LC286">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">,</span><span class="nc">symbol</span> <span class="o">,</span><span class="nb">position</span> <span class="p">(</span><span class="nb">nreverse</span> <span class="vg">*nonterminal-stack*</span><span class="p">)))</span></div><div class="line" id="LC287">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="o">,</span><span class="nv">result</span></div><div class="line" id="LC288">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">,</span><span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC289">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="no">t</span></div><div class="line" id="LC290">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; First mark this pair with T to detect left-recursion,</span></div><div class="line" id="LC291">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; then compute the result and cache that.</span></div><div class="line" id="LC292">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nv">get-cached</span> <span class="o">,</span><span class="nc">symbol</span> <span class="o">,</span><span class="nb">position</span> <span class="o">,</span><span class="nv">cache</span><span class="p">)</span> <span class="no">t</span></div><div class="line" id="LC293">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">get-cached</span> <span class="o">,</span><span class="nc">symbol</span> <span class="o">,</span><span class="nb">position</span> <span class="o">,</span><span class="nv">cache</span><span class="p">)</span> <span class="p">(</span><span class="k">locally</span> <span class="o">,@</span><span class="nv">forms</span><span class="p">)))))))</span></div><div class="line" id="LC294">&nbsp;</div><div class="line" id="LC295"><span class="c1">;;; RESULT REPRESENTATION</span></div><div class="line" id="LC296"><span class="c1">;;;</span></div><div class="line" id="LC297"><span class="c1">;;; We always return a result -- ERROR-RESULT for failed parses, and</span></div><div class="line" id="LC298"><span class="c1">;;; RESULT for successes.</span></div><div class="line" id="LC299"><span class="c1">;;;</span></div><div class="line" id="LC300"><span class="c1">;;; We implement a simple lazy evaluation for the productions. This is</span></div><div class="line" id="LC301"><span class="c1">;;; used to perform semantic actions only when necessary -- either</span></div><div class="line" id="LC302"><span class="c1">;;; when we call a semantic predicate or once parse has finished.</span></div><div class="line" id="LC303">&nbsp;</div><div class="line" id="LC304"><span class="p">(</span><span class="nb">defstruct</span> <span class="nv">error-result</span></div><div class="line" id="LC305">&nbsp;&nbsp;<span class="c1">;; Expression that failed to match.</span></div><div class="line" id="LC306">&nbsp;&nbsp;<span class="nv">expression</span></div><div class="line" id="LC307">&nbsp;&nbsp;<span class="c1">;; Position at which match was attempted.</span></div><div class="line" id="LC308">&nbsp;&nbsp;<span class="p">(</span><span class="nb">position</span> <span class="p">(</span><span class="nv">required-argument</span><span class="p">)</span> <span class="ss">:type</span> <span class="nv">array-index</span><span class="p">)</span></div><div class="line" id="LC309">&nbsp;&nbsp;<span class="c1">;; A nested error, closer to actual failure site.</span></div><div class="line" id="LC310">&nbsp;&nbsp;<span class="nv">detail</span><span class="p">)</span></div><div class="line" id="LC311">&nbsp;</div><div class="line" id="LC312"><span class="p">(</span><span class="nb">defstruct</span> <span class="p">(</span><span class="nv">result</span> <span class="p">(</span><span class="ss">:constructor</span> <span class="nv">%make-result</span><span class="p">))</span></div><div class="line" id="LC313">&nbsp;&nbsp;<span class="c1">;; Either a list of results, whose first element is the production, or a</span></div><div class="line" id="LC314">&nbsp;&nbsp;<span class="c1">;; function to call that will return the production.</span></div><div class="line" id="LC315">&nbsp;&nbsp;<span class="nv">%production</span></div><div class="line" id="LC316">&nbsp;&nbsp;<span class="c1">;; Position after the match.</span></div><div class="line" id="LC317">&nbsp;&nbsp;<span class="p">(</span><span class="nb">position</span> <span class="p">(</span><span class="nv">required-argument</span><span class="p">)</span> <span class="ss">:type</span> <span class="nv">array-index</span><span class="p">))</span></div><div class="line" id="LC318">&nbsp;</div><div class="line" id="LC319"><span class="p">(</span><span class="nb">defmacro</span> <span class="nv">make-result</span> <span class="p">(</span><span class="k">&amp;rest</span> <span class="nv">arguments</span> <span class="k">&amp;key</span> <span class="nv">production</span> <span class="k">&amp;allow-other-keys</span><span class="p">)</span></div><div class="line" id="LC320">&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="nv">production</span></div><div class="line" id="LC321">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">args</span> <span class="p">(</span><span class="nb">copy-list</span> <span class="nv">arguments</span><span class="p">)))</span></div><div class="line" id="LC322">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">remf</span> <span class="nv">args</span> <span class="ss">:production</span><span class="p">)</span></div><div class="line" id="LC323">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">`</span><span class="p">(</span><span class="nv">%make-result</span> <span class="o">,@</span><span class="nv">args</span></div><div class="line" id="LC324">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:%production</span> <span class="o">,</span><span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">symbolp</span> <span class="nv">production</span><span class="p">)</span></div><div class="line" id="LC325">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">`</span><span class="p">(</span><span class="nb">list</span> <span class="o">,</span><span class="nv">production</span><span class="p">)</span></div><div class="line" id="LC326">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">`</span><span class="p">(</span><span class="k">lambda</span> <span class="p">()</span> <span class="o">,</span><span class="nv">production</span><span class="p">))))</span></div><div class="line" id="LC327">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">`</span><span class="p">(</span><span class="nv">%make-result</span> <span class="o">,@</span><span class="nv">arguments</span><span class="p">)))</span></div><div class="line" id="LC328">&nbsp;</div><div class="line" id="LC329"><span class="p">(</span><span class="nb">defun</span> <span class="nv">result-production</span> <span class="p">(</span><span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC330">&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">thunk</span> <span class="p">(</span><span class="nv">result-%production</span> <span class="nv">result</span><span class="p">)))</span></div><div class="line" id="LC331">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">functionp</span> <span class="nv">thunk</span><span class="p">)</span></div><div class="line" id="LC332">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">value</span> <span class="p">(</span><span class="nb">funcall</span> <span class="nv">thunk</span><span class="p">)))</span></div><div class="line" id="LC333">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nv">result-%production</span> <span class="nv">result</span><span class="p">)</span> <span class="p">(</span><span class="nb">list</span> <span class="nv">value</span><span class="p">))</span></div><div class="line" id="LC334">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">value</span><span class="p">)</span></div><div class="line" id="LC335">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">car</span> <span class="nv">thunk</span><span class="p">))))</span></div><div class="line" id="LC336">&nbsp;</div><div class="line" id="LC337"><span class="c1">;;; MAIN INTERFACE</span></div><div class="line" id="LC338">&nbsp;</div><div class="line" id="LC339"><span class="p">(</span><span class="nb">defun</span> <span class="nv">parse</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">text</span> <span class="k">&amp;key</span> <span class="p">(</span><span class="nv">start</span> <span class="mi">0</span><span class="p">)</span> <span class="nv">end</span> <span class="nv">junk-allowed</span><span class="p">)</span></div><div class="line" id="LC340">&nbsp;&nbsp;<span class="c1">;; There is no backtracking in the toplevel expression -- so there&#39;s</span></div><div class="line" id="LC341">&nbsp;&nbsp;<span class="c1">;; no point in compiling it as it will be executed only once -- unless</span></div><div class="line" id="LC342">&nbsp;&nbsp;<span class="c1">;; it&#39;s a constant, for which we have a compiler-macro.</span></div><div class="line" id="LC343">&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">end</span> <span class="p">(</span><span class="nb">or</span> <span class="nv">end</span> <span class="p">(</span><span class="nb">length</span> <span class="nv">text</span><span class="p">))))</span></div><div class="line" id="LC344">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">process-parse-result</span></div><div class="line" id="LC345">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="vg">*cache*</span> <span class="p">(</span><span class="nv">make-cache</span><span class="p">)))</span></div><div class="line" id="LC346">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-expression</span> <span class="nv">expression</span> <span class="nv">text</span> <span class="nv">start</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC347">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">end</span></div><div class="line" id="LC348">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">junk-allowed</span><span class="p">)))</span></div><div class="line" id="LC349">&nbsp;</div><div class="line" id="LC350"><span class="p">(</span><span class="nb">define-compiler-macro</span> <span class="nv">parse</span> <span class="p">(</span><span class="k">&amp;whole</span> <span class="nv">form</span> <span class="nv">expression</span> <span class="k">&amp;rest</span> <span class="nv">arguments</span></div><div class="line" id="LC351">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">&amp;environment</span> <span class="nv">env</span><span class="p">)</span></div><div class="line" id="LC352">&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">constantp</span> <span class="nv">expression</span> <span class="nv">env</span><span class="p">)</span></div><div class="line" id="LC353">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-gensyms</span> <span class="p">(</span><span class="nv">expr-fun</span><span class="p">)</span></div><div class="line" id="LC354">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">`</span><span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="o">,</span><span class="nv">expr-fun</span> <span class="p">(</span><span class="k">load-time-value</span> <span class="p">(</span><span class="nv">compile-expression</span> <span class="o">,</span><span class="nv">expression</span><span class="p">))))</span></div><div class="line" id="LC355">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; This inline-lambda here provides keyword defaults and</span></div><div class="line" id="LC356">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; parsing, so the compiler-macro doesn&#39;t have to worry</span></div><div class="line" id="LC357">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; about evaluation order.</span></div><div class="line" id="LC358">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="k">lambda</span> <span class="p">(</span><span class="nv">text</span> <span class="k">&amp;key</span> <span class="p">(</span><span class="nv">start</span> <span class="mi">0</span><span class="p">)</span> <span class="nv">end</span> <span class="nv">junk-allowed</span><span class="p">)</span></div><div class="line" id="LC359">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="vg">*cache*</span> <span class="p">(</span><span class="nv">make-cache</span><span class="p">))</span></div><div class="line" id="LC360">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">end</span> <span class="p">(</span><span class="nb">or</span> <span class="nv">end</span> <span class="p">(</span><span class="nb">length</span> <span class="nv">text</span><span class="p">))))</span></div><div class="line" id="LC361">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">process-parse-result</span></div><div class="line" id="LC362">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">funcall</span> <span class="o">,</span><span class="nv">expr-fun</span> <span class="nv">text</span> <span class="nv">start</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC363">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">end</span></div><div class="line" id="LC364">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">junk-allowed</span><span class="p">)))</span></div><div class="line" id="LC365">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">,@</span><span class="nv">arguments</span><span class="p">)))</span></div><div class="line" id="LC366">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">form</span><span class="p">))</span></div><div class="line" id="LC367">&nbsp;</div><div class="line" id="LC368"><span class="p">(</span><span class="nb">defun</span> <span class="nv">process-parse-result</span> <span class="p">(</span><span class="nv">result</span> <span class="nv">end</span> <span class="nv">junk-allowed</span><span class="p">)</span></div><div class="line" id="LC369">&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC370">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="nv">junk-allowed</span></div><div class="line" id="LC371">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">values</span> <span class="no">nil</span> <span class="mi">0</span><span class="p">)</span></div><div class="line" id="LC372">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">error</span> <span class="p">(</span><span class="nb">with-output-to-string</span> <span class="p">(</span><span class="nv">s</span><span class="p">)</span></div><div class="line" id="LC373">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">format</span> <span class="nv">s</span> <span class="s">&quot;Expression ~S failed at ~S~:[.~;:~]&quot;</span> </div><div class="line" id="LC374">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">error-result-expression</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC375">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">error-result-position</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC376">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">error-result-detail</span> <span class="nv">result</span><span class="p">))</span></div><div class="line" id="LC377">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">labels</span> <span class="p">((</span><span class="nv">rec</span> <span class="p">(</span><span class="nv">e</span><span class="p">)</span></div><div class="line" id="LC378">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="nv">e</span></div><div class="line" id="LC379">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">format</span> <span class="nv">s</span> <span class="s">&quot;~&amp; subexpression ~S failed at ~S.&quot;</span></div><div class="line" id="LC380">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">error-result-expression</span> <span class="nv">e</span><span class="p">)</span></div><div class="line" id="LC381">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">error-result-position</span> <span class="nv">e</span><span class="p">))</span> </div><div class="line" id="LC382">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">rec</span> <span class="p">(</span><span class="nv">error-result-detail</span> <span class="nv">e</span><span class="p">)))))</span></div><div class="line" id="LC383">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">rec</span> <span class="p">(</span><span class="nv">error-result-detail</span> <span class="nv">result</span><span class="p">))))))</span></div><div class="line" id="LC384">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nb">position</span> <span class="p">(</span><span class="nv">result-position</span> <span class="nv">result</span><span class="p">)))</span></div><div class="line" id="LC385">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">values</span> <span class="p">(</span><span class="nv">result-production</span> <span class="nv">result</span><span class="p">)</span> </div><div class="line" id="LC386">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="p">(</span><span class="nb">&lt;</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC387">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="nv">junk-allowed</span></div><div class="line" id="LC388">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nb">position</span></div><div class="line" id="LC389">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">error</span> <span class="s">&quot;Incomplete parse, stopped at ~S.&quot;</span> <span class="nb">position</span><span class="p">)))))))</span></div><div class="line" id="LC390">&nbsp;</div><div class="line" id="LC391"><span class="p">(</span><span class="nb">defmacro</span> <span class="nv">defrule</span> <span class="p">(</span><span class="k">&amp;whole</span> <span class="nv">form</span> <span class="nc">symbol</span> <span class="nv">expression</span> <span class="k">&amp;body</span> <span class="nv">options</span><span class="p">)</span></div><div class="line" id="LC392">&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">(</span><span class="nv">transform</span><span class="p">)</span></div><div class="line" id="LC393">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="nv">options</span></div><div class="line" id="LC394">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dolist</span> <span class="p">(</span><span class="nv">option</span> <span class="nv">options</span><span class="p">)</span></div><div class="line" id="LC395">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="nv">transform</span></div><div class="line" id="LC396">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">error</span> <span class="s">&quot;Multiple transforms in DEFRULE:~% ~S&quot;</span> <span class="nv">form</span><span class="p">))</span></div><div class="line" id="LC397">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">ecase</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">option</span><span class="p">)</span></div><div class="line" id="LC398">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="ss">:constant</span> </div><div class="line" id="LC399">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="nv">transform</span> <span class="o">`</span><span class="p">(</span><span class="k">lambda</span> <span class="p">(</span><span class="nv">x</span><span class="p">)</span> <span class="p">(</span><span class="k">declare</span> <span class="p">(</span><span class="k">ignore</span> <span class="nv">x</span><span class="p">))</span> <span class="o">,</span><span class="p">(</span><span class="nb">second</span> <span class="nv">option</span><span class="p">))))</span></div><div class="line" id="LC400">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="ss">:concat</span></div><div class="line" id="LC401">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="p">(</span><span class="nb">second</span> <span class="nv">option</span><span class="p">)</span></div><div class="line" id="LC402">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="nv">transform</span> <span class="o">&#39;</span><span class="nf">#&#39;</span><span class="nv">concat</span><span class="p">)))</span></div><div class="line" id="LC403">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="ss">:lambda</span></div><div class="line" id="LC404">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">destructuring-bind</span> <span class="p">(</span><span class="nv">lambda-list</span> <span class="k">&amp;body</span> <span class="nv">forms</span><span class="p">)</span> <span class="p">(</span><span class="nb">cdr</span> <span class="nv">option</span><span class="p">)</span></div><div class="line" id="LC405">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="nv">transform</span> <span class="o">`</span><span class="p">(</span><span class="k">lambda</span> <span class="o">,</span><span class="nv">lambda-list</span> <span class="o">,@</span><span class="nv">forms</span><span class="p">))))</span></div><div class="line" id="LC406">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="ss">:destructure</span></div><div class="line" id="LC407">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">destructuring-bind</span> <span class="p">(</span><span class="nv">lambda-list</span> <span class="k">&amp;body</span> <span class="nv">forms</span><span class="p">)</span> <span class="p">(</span><span class="nb">cdr</span> <span class="nv">option</span><span class="p">)</span></div><div class="line" id="LC408">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="nv">transform</span></div><div class="line" id="LC409">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-gensyms</span> <span class="p">(</span><span class="nv">production</span><span class="p">)</span></div><div class="line" id="LC410">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">`</span><span class="p">(</span><span class="k">lambda</span> <span class="p">(</span><span class="o">,</span><span class="nv">production</span><span class="p">)</span></div><div class="line" id="LC411">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">destructuring-bind</span> <span class="o">,</span><span class="nv">lambda-list</span> <span class="o">,</span><span class="nv">production</span></div><div class="line" id="LC412">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">,@</span><span class="nv">forms</span><span class="p">)))))))))</span></div><div class="line" id="LC413">&nbsp;&nbsp;&nbsp;&nbsp;<span class="o">`</span><span class="p">(</span><span class="k">eval-when</span> <span class="p">(</span><span class="ss">:load-toplevel</span> <span class="ss">:execute</span><span class="p">)</span></div><div class="line" id="LC414">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">add-rule</span> <span class="ss">&#39;,symbol</span> <span class="p">(</span><span class="nb">make-instance</span> <span class="ss">&#39;rule</span></div><div class="line" id="LC415">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="ss">&#39;,expression</span></div><div class="line" id="LC416">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:transform</span> <span class="o">,</span><span class="nv">transform</span><span class="p">)))))</span></div><div class="line" id="LC417">&nbsp;</div><div class="line" id="LC418"><span class="p">(</span><span class="nb">defun</span> <span class="nv">add-rule</span> <span class="p">(</span><span class="nc">symbol</span> <span class="nv">rule</span><span class="p">)</span></div><div class="line" id="LC419">&nbsp;&nbsp;<span class="s">&quot;Associates RULE with the nonterminal SYMBOL. Signals an error if the</span></div><div class="line" id="LC420"><span class="s">rule is already associated with a nonterminal. If the symbol is already</span></div><div class="line" id="LC421"><span class="s">associated with a rule, the old rule is removed first.&quot;</span></div><div class="line" id="LC422">&nbsp;&nbsp;<span class="c1">;; FIXME: This needs locking and WITHOUT-INTERRUPTS.</span></div><div class="line" id="LC423">&nbsp;&nbsp;<span class="p">(</span><span class="nb">check-type</span> <span class="nc">symbol</span> <span class="nv">nonterminal</span><span class="p">)</span></div><div class="line" id="LC424">&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="p">(</span><span class="nv">rule-symbol</span> <span class="nv">rule</span><span class="p">)</span></div><div class="line" id="LC425">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">error</span> <span class="s">&quot;~S is already associated with the nonterminal ~S -- remove it first.&quot;</span></div><div class="line" id="LC426">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">rule</span> <span class="p">(</span><span class="nv">rule-symbol</span> <span class="nv">rule</span><span class="p">)))</span></div><div class="line" id="LC427">&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">cell</span> <span class="p">(</span><span class="nv">ensure-rule-cell</span> <span class="nc">symbol</span><span class="p">))</span></div><div class="line" id="LC428">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">function</span> <span class="p">(</span><span class="nv">compile-rule</span> <span class="nc">symbol</span> </div><div class="line" id="LC429">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">rule-expression</span> <span class="nv">rule</span><span class="p">)</span></div><div class="line" id="LC430">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">rule-transform</span> <span class="nv">rule</span><span class="p">))))</span></div><div class="line" id="LC431">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nv">cell-function</span> <span class="nv">cell</span><span class="p">)</span> <span class="k">function</span></div><div class="line" id="LC432">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">cell-rule</span> <span class="nv">cell</span><span class="p">)</span> <span class="nv">rule</span></div><div class="line" id="LC433">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">slot-value</span> <span class="nv">rule</span> <span class="ss">&#39;%symbol</span><span class="p">)</span> <span class="nc">symbol</span><span class="p">)</span></div><div class="line" id="LC434">&nbsp;&nbsp;&nbsp;&nbsp;<span class="nc">symbol</span><span class="p">))</span></div><div class="line" id="LC435">&nbsp;</div><div class="line" id="LC436"><span class="p">(</span><span class="nb">defun</span> <span class="nv">find-rule</span> <span class="p">(</span><span class="nc">symbol</span><span class="p">)</span></div><div class="line" id="LC437">&nbsp;&nbsp;<span class="s">&quot;Returns rule designated by SYMBOL, if any. Symbol must be a nonterminal</span></div><div class="line" id="LC438"><span class="s">symbol.&quot;</span></div><div class="line" id="LC439">&nbsp;&nbsp;<span class="p">(</span><span class="nb">check-type</span> <span class="nc">symbol</span> <span class="nv">nonterminal</span><span class="p">)</span></div><div class="line" id="LC440">&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">cell</span> <span class="p">(</span><span class="nv">find-rule-cell</span> <span class="nc">symbol</span><span class="p">)))</span></div><div class="line" id="LC441">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="nv">cell</span></div><div class="line" id="LC442">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cddr</span> <span class="nv">cell</span><span class="p">))))</span></div><div class="line" id="LC443">&nbsp;</div><div class="line" id="LC444"><span class="p">(</span><span class="nb">defun</span> <span class="nv">remove-rule</span> <span class="p">(</span><span class="nc">symbol</span> <span class="k">&amp;key</span> <span class="nv">force</span><span class="p">)</span></div><div class="line" id="LC445">&nbsp;&nbsp;<span class="s">&quot;Makes the nonterminal SYMBOL undefined. If the nonterminal is defined an</span></div><div class="line" id="LC446"><span class="s">already referred to by other rules, an error is signalled unless :FORCE is</span></div><div class="line" id="LC447"><span class="s">true.&quot;</span></div><div class="line" id="LC448">&nbsp;&nbsp;<span class="p">(</span><span class="nb">check-type</span> <span class="nc">symbol</span> <span class="nv">nonterminal</span><span class="p">)</span></div><div class="line" id="LC449">&nbsp;&nbsp;<span class="c1">;; FIXME: Lock and WITHOUT-INTERRUPTS.</span></div><div class="line" id="LC450">&nbsp;&nbsp;<span class="p">(</span><span class="k">let*</span> <span class="p">((</span><span class="nv">cell</span> <span class="p">(</span><span class="nv">find-rule-cell</span> <span class="nc">symbol</span><span class="p">))</span></div><div class="line" id="LC451">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">rule</span> <span class="p">(</span><span class="nv">cell-rule</span> <span class="nv">cell</span><span class="p">)))</span></div><div class="line" id="LC452">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="nv">cell</span></div><div class="line" id="LC453">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cond</span> <span class="p">((</span><span class="nb">and</span> <span class="nv">rule</span> <span class="p">(</span><span class="nv">cell-referents</span> <span class="nv">cell</span><span class="p">))</span></div><div class="line" id="LC454">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">unless</span> <span class="nv">force</span></div><div class="line" id="LC455">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">error</span> <span class="s">&quot;Nonterminal ~S is used by other nonterminal~P:~% ~{~S~^, ~}&quot;</span></div><div class="line" id="LC456">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nc">symbol</span> <span class="p">(</span><span class="nb">length</span> <span class="p">(</span><span class="nv">cell-referents</span> <span class="nv">cell</span><span class="p">))</span> <span class="p">(</span><span class="nv">cell-referents</span> <span class="nv">cell</span><span class="p">)))</span></div><div class="line" id="LC457">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nv">cell-function</span> <span class="nv">cell</span><span class="p">)</span> <span class="p">(</span><span class="k">lambda</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC458">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">declare</span> <span class="p">(</span><span class="k">ignore</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC459">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">error</span> <span class="s">&quot;Undefined rule: ~S&quot;</span> <span class="nc">symbol</span><span class="p">))</span></div><div class="line" id="LC460">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">cell-rule</span> <span class="nv">cell</span><span class="p">)</span> <span class="no">nil</span><span class="p">)</span></div><div class="line" id="LC461">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">detach-rule</span> <span class="nv">rule</span><span class="p">))</span></div><div class="line" id="LC462">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">not</span> <span class="p">(</span><span class="nv">cell-referents</span> <span class="nv">cell</span><span class="p">))</span></div><div class="line" id="LC463">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="nv">rule</span></div><div class="line" id="LC464">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">detach-rule</span> <span class="nv">rule</span><span class="p">))</span></div><div class="line" id="LC465">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; There are no references to the rule at all, so</span></div><div class="line" id="LC466">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; we can remove the cell.</span></div><div class="line" id="LC467">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">delete-rule-cell</span> <span class="nc">symbol</span><span class="p">)))</span></div><div class="line" id="LC468">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">rule</span><span class="p">)))</span></div><div class="line" id="LC469">&nbsp;</div><div class="line" id="LC470"><span class="p">(</span><span class="nb">defun</span> <span class="nv">symbol-length</span> <span class="p">(</span><span class="nv">x</span><span class="p">)</span></div><div class="line" id="LC471">&nbsp;&nbsp;<span class="p">(</span><span class="nb">length</span> <span class="p">(</span><span class="nb">symbol-name</span> <span class="nv">x</span><span class="p">)))</span></div><div class="line" id="LC472">&nbsp;</div><div class="line" id="LC473"><span class="p">(</span><span class="nb">defun</span> <span class="nv">describe-grammar</span> <span class="p">(</span><span class="nc">symbol</span> <span class="k">&amp;optional</span> <span class="p">(</span><span class="nc">stream</span> <span class="vg">*standard-output*</span><span class="p">))</span>  </div><div class="line" id="LC474">&nbsp;&nbsp;<span class="s">&quot;Prints the grammar tree rooted at nonterminal SYMBOL to STREAM for human</span></div><div class="line" id="LC475"><span class="s">inspection.&quot;</span></div><div class="line" id="LC476">&nbsp;&nbsp;<span class="p">(</span><span class="nb">check-type</span> <span class="nc">symbol</span> <span class="nv">nonterminal</span><span class="p">)</span></div><div class="line" id="LC477">&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">rule</span> <span class="p">(</span><span class="nv">find-rule</span> <span class="nc">symbol</span><span class="p">)))</span></div><div class="line" id="LC478">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cond</span> <span class="p">((</span><span class="nb">not</span> <span class="nv">rule</span><span class="p">)</span></div><div class="line" id="LC479">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">format</span> <span class="nc">stream</span> <span class="s">&quot;Symbol ~S is not a defined nonterminal.&quot;</span> <span class="nc">symbol</span><span class="p">))</span></div><div class="line" id="LC480">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="no">t</span></div><div class="line" id="LC481">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">format</span> <span class="nc">stream</span> <span class="s">&quot;~&amp;Grammar ~S:~%&quot;</span> <span class="nc">symbol</span><span class="p">)</span></div><div class="line" id="LC482">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">multiple-value-bind</span> <span class="p">(</span><span class="nv">defined</span> <span class="nv">undefined</span><span class="p">)</span> <span class="p">(</span><span class="nv">rule-dependencies</span> <span class="nv">rule</span><span class="p">)</span></div><div class="line" id="LC483">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nb">length</span> </div><div class="line" id="LC484">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">+</span> <span class="mi">4</span> <span class="p">(</span><span class="nb">max</span> <span class="p">(</span><span class="nb">reduce</span> <span class="nf">#&#39;</span><span class="nb">max</span> <span class="p">(</span><span class="nb">mapcar</span> <span class="nf">#&#39;</span><span class="nv">symbol-length</span> <span class="nv">defined</span><span class="p">)</span></div><div class="line" id="LC485">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:initial-value</span> <span class="mi">0</span><span class="p">)</span></div><div class="line" id="LC486">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">reduce</span> <span class="nf">#&#39;</span><span class="nb">max</span> <span class="p">(</span><span class="nb">mapcar</span> <span class="nf">#&#39;</span><span class="nv">symbol-length</span> <span class="nv">undefined</span><span class="p">)</span></div><div class="line" id="LC487">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:initial-value</span> <span class="mi">0</span><span class="p">)))))</span></div><div class="line" id="LC488">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">format</span> <span class="nc">stream</span> <span class="s">&quot;~3T~S~VT&lt;- ~S~%&quot;</span> </div><div class="line" id="LC489">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nc">symbol</span> <span class="nb">length</span> <span class="p">(</span><span class="nv">rule-expression</span> <span class="nv">rule</span><span class="p">))</span></div><div class="line" id="LC490">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="nv">defined</span></div><div class="line" id="LC491">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dolist</span> <span class="p">(</span><span class="nv">s</span> <span class="nv">defined</span><span class="p">)</span></div><div class="line" id="LC492">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">format</span> <span class="nc">stream</span> <span class="s">&quot;~3T~S~VT&lt;- ~S~%&quot;</span> </div><div class="line" id="LC493">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">s</span> <span class="nb">length</span> <span class="p">(</span><span class="nv">rule-expression</span> <span class="p">(</span><span class="nv">find-rule</span> <span class="nv">s</span><span class="p">)))))</span></div><div class="line" id="LC494">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="nv">undefined</span></div><div class="line" id="LC495">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">format</span> <span class="nc">stream</span> <span class="s">&quot;~%Undefined nonterminal~P:~%~{~3T~S~%~}&quot;</span></div><div class="line" id="LC496">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">length</span> <span class="nv">undefined</span><span class="p">)</span> <span class="nv">undefined</span><span class="p">))))))))</span></div><div class="line" id="LC497">&nbsp;</div><div class="line" id="LC498"><span class="c1">;;; COMPILING RULES</span></div><div class="line" id="LC499">&nbsp;</div><div class="line" id="LC500"><span class="p">(</span><span class="nb">defvar</span> <span class="vg">*current-rule*</span><span class="p">)</span></div><div class="line" id="LC501">&nbsp;</div><div class="line" id="LC502"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-rule</span> <span class="p">(</span><span class="nc">symbol</span> <span class="nv">expression</span> <span class="nv">transform</span><span class="p">)</span></div><div class="line" id="LC503">&nbsp;&nbsp;<span class="p">(</span><span class="k">let*</span> <span class="p">((</span><span class="vg">*current-rule*</span> <span class="nc">symbol</span><span class="p">)</span></div><div class="line" id="LC504">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">function</span> <span class="p">(</span><span class="nv">compile-expression</span> <span class="nv">expression</span><span class="p">)))</span></div><div class="line" id="LC505">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="nv">transform</span></div><div class="line" id="LC506">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">named-lambda</span> <span class="nv">rule</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC507">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-cached-result</span> <span class="p">(</span><span class="nc">symbol</span> <span class="nb">position</span><span class="p">)</span></div><div class="line" id="LC508">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nb">funcall</span> <span class="k">function</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC509">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC510">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span> </div><div class="line" id="LC511">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nc">symbol</span></div><div class="line" id="LC512">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC513">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:detail</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC514">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span></div><div class="line" id="LC515">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="p">(</span><span class="nv">result-position</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC516">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:production</span> <span class="p">(</span><span class="nb">funcall</span> <span class="nv">transform</span> <span class="p">(</span><span class="nv">result-production</span> <span class="nv">result</span><span class="p">)))))))</span></div><div class="line" id="LC517">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">lambda</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC518">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-cached-result</span> <span class="p">(</span><span class="nc">symbol</span> <span class="nb">position</span><span class="p">)</span></div><div class="line" id="LC519">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">funcall</span> <span class="k">function</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))))))</span></div><div class="line" id="LC520">&nbsp;</div><div class="line" id="LC521"><span class="c1">;;; EXPRESSION COMPILER &amp; EVALUATOR</span></div><div class="line" id="LC522">&nbsp;</div><div class="line" id="LC523"><span class="p">(</span><span class="nb">defun</span> <span class="nv">invalid-expression-error</span> <span class="p">(</span><span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC524">&nbsp;&nbsp;<span class="p">(</span><span class="nb">error</span> <span class="s">&quot;Invalid expression: ~S&quot;</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC525">&nbsp;</div><div class="line" id="LC526"><span class="p">(</span><span class="nb">defun</span> <span class="nv">validate-expression</span> <span class="p">(</span><span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC527">&nbsp;&nbsp;<span class="p">(</span><span class="nb">or</span> <span class="p">(</span><span class="nb">typecase</span> <span class="nv">expression</span></div><div class="line" id="LC528">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">eql</span> <span class="nb">character</span><span class="p">)</span></div><div class="line" id="LC529">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="no">t</span><span class="p">)</span></div><div class="line" id="LC530">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">terminal</span></div><div class="line" id="LC531">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="no">t</span><span class="p">)</span></div><div class="line" id="LC532">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">nonterminal</span></div><div class="line" id="LC533">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="no">t</span><span class="p">)</span></div><div class="line" id="LC534">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cons</span></div><div class="line" id="LC535">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">case</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC536">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">and</span> <span class="nb">or</span><span class="p">)</span></div><div class="line" id="LC537">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">and</span> <span class="p">(</span><span class="nb">every</span> <span class="nf">#&#39;</span><span class="nv">validate-expression</span> <span class="p">(</span><span class="nb">cdr</span> <span class="nv">expression</span><span class="p">))</span> <span class="no">t</span><span class="p">))</span></div><div class="line" id="LC538">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="no">nil</span><span class="p">)</span></div><div class="line" id="LC539">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="no">nil</span><span class="p">)</span></div><div class="line" id="LC540">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">string</span></div><div class="line" id="LC541">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">and</span> <span class="p">(</span><span class="nb">cdr</span> <span class="nv">expression</span><span class="p">)</span> <span class="p">(</span><span class="nb">not</span> <span class="p">(</span><span class="nb">cddr</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC542">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">typep</span> <span class="p">(</span><span class="nb">second</span> <span class="nv">expression</span><span class="p">)</span> <span class="ss">&#39;array-length</span><span class="p">)))</span></div><div class="line" id="LC543">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="no">t</span></div><div class="line" id="LC544">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">and</span> <span class="p">(</span><span class="nb">symbolp</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC545">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cdr</span> <span class="nv">expression</span><span class="p">)</span> <span class="p">(</span><span class="nb">not</span> <span class="p">(</span><span class="nb">cddr</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC546">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">validate-expression</span> <span class="p">(</span><span class="nb">second</span> <span class="nv">expression</span><span class="p">))))))</span></div><div class="line" id="LC547">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="no">t</span></div><div class="line" id="LC548">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="no">nil</span><span class="p">))</span></div><div class="line" id="LC549">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">invalid-expression-error</span> <span class="nv">expression</span><span class="p">)))</span></div><div class="line" id="LC550">&nbsp;</div><div class="line" id="LC551"><span class="p">(</span><span class="nb">defun</span> <span class="nv">%expression-dependencies</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">seen</span><span class="p">)</span></div><div class="line" id="LC552">&nbsp;&nbsp;<span class="p">(</span><span class="nb">etypecase</span> <span class="nv">expression</span></div><div class="line" id="LC553">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">member</span> <span class="nb">character</span><span class="p">)</span></div><div class="line" id="LC554">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">seen</span><span class="p">)</span></div><div class="line" id="LC555">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">terminal</span></div><div class="line" id="LC556">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">seen</span><span class="p">)</span></div><div class="line" id="LC557">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">nonterminal</span></div><div class="line" id="LC558">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">member</span> <span class="nv">expression</span> <span class="nv">seen</span> <span class="ss">:test</span> <span class="nf">#&#39;</span><span class="nb">eq</span><span class="p">)</span></div><div class="line" id="LC559">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">seen</span></div><div class="line" id="LC560">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">rule</span> <span class="p">(</span><span class="nv">find-rule</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC561">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">seen</span> <span class="p">(</span><span class="nb">cons</span> <span class="nv">expression</span> <span class="nv">seen</span><span class="p">)))</span></div><div class="line" id="LC562">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="nv">rule</span></div><div class="line" id="LC563">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">%expression-dependencies</span> <span class="p">(</span><span class="nv">rule-expression</span> <span class="nv">rule</span><span class="p">)</span> <span class="nv">seen</span><span class="p">)</span></div><div class="line" id="LC564">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">seen</span><span class="p">))))</span></div><div class="line" id="LC565">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cons</span></div><div class="line" id="LC566">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">case</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC567">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">string</span></div><div class="line" id="LC568">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">seen</span><span class="p">)</span></div><div class="line" id="LC569">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">and</span> <span class="nb">or</span><span class="p">)</span></div><div class="line" id="LC570">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dolist</span> <span class="p">(</span><span class="nv">subexpr</span> <span class="p">(</span><span class="nb">cdr</span> <span class="nv">expression</span><span class="p">)</span> <span class="nv">seen</span><span class="p">)</span></div><div class="line" id="LC571">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="nv">seen</span> <span class="p">(</span><span class="nv">%expression-dependencies</span> <span class="nv">subexpr</span> <span class="nv">seen</span><span class="p">))))</span></div><div class="line" id="LC572">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">*</span> <span class="nb">+</span> <span class="nv">?</span> <span class="nv">&amp;</span> <span class="nv">!</span><span class="p">)</span></div><div class="line" id="LC573">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">%expression-dependencies</span> <span class="p">(</span><span class="nb">second</span> <span class="nv">expression</span><span class="p">)</span> <span class="nv">seen</span><span class="p">))</span></div><div class="line" id="LC574">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="no">t</span></div><div class="line" id="LC575">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">%expression-dependencies</span> <span class="p">(</span><span class="nb">second</span> <span class="nv">expression</span><span class="p">)</span> <span class="nv">seen</span><span class="p">))))))</span></div><div class="line" id="LC576">&nbsp;</div><div class="line" id="LC577"><span class="p">(</span><span class="nb">defun</span> <span class="nv">%expression-direct-dependencies</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">seen</span><span class="p">)</span></div><div class="line" id="LC578">&nbsp;&nbsp;<span class="p">(</span><span class="nb">etypecase</span> <span class="nv">expression</span></div><div class="line" id="LC579">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">member</span> <span class="nb">character</span><span class="p">)</span></div><div class="line" id="LC580">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">seen</span><span class="p">)</span></div><div class="line" id="LC581">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">terminal</span></div><div class="line" id="LC582">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">seen</span><span class="p">)</span></div><div class="line" id="LC583">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">nonterminal</span></div><div class="line" id="LC584">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cons</span> <span class="nv">expression</span> <span class="nv">seen</span><span class="p">))</span></div><div class="line" id="LC585">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cons</span></div><div class="line" id="LC586">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">case</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC587">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">string</span></div><div class="line" id="LC588">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">seen</span><span class="p">)</span></div><div class="line" id="LC589">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">and</span> <span class="nb">or</span><span class="p">)</span></div><div class="line" id="LC590">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dolist</span> <span class="p">(</span><span class="nv">subexpr</span> <span class="p">(</span><span class="nb">cdr</span> <span class="nv">expression</span><span class="p">)</span> <span class="nv">seen</span><span class="p">)</span></div><div class="line" id="LC591">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="nv">seen</span> <span class="p">(</span><span class="nv">%expression-direct-dependencies</span> <span class="nv">subexpr</span> <span class="nv">seen</span><span class="p">))))</span></div><div class="line" id="LC592">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">*</span> <span class="nb">+</span> <span class="nv">?</span> <span class="nv">&amp;</span> <span class="nv">!</span><span class="p">)</span></div><div class="line" id="LC593">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">%expression-direct-dependencies</span> <span class="p">(</span><span class="nb">second</span> <span class="nv">expression</span><span class="p">)</span> <span class="nv">seen</span><span class="p">))</span></div><div class="line" id="LC594">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="no">t</span></div><div class="line" id="LC595">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">%expression-direct-dependencies</span> <span class="p">(</span><span class="nb">second</span> <span class="nv">expression</span><span class="p">)</span> <span class="nv">seen</span><span class="p">))))))</span></div><div class="line" id="LC596">&nbsp;</div><div class="line" id="LC597"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC598">&nbsp;&nbsp;<span class="p">(</span><span class="nb">typecase</span> <span class="nv">expression</span></div><div class="line" id="LC599">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">eql</span> <span class="nb">character</span><span class="p">)</span></div><div class="line" id="LC600">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-character</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC601">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">terminal</span></div><div class="line" id="LC602">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-terminal</span> <span class="p">(</span><span class="nb">string</span> <span class="nv">expression</span><span class="p">)</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span>    </div><div class="line" id="LC603">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">nonterminal</span></div><div class="line" id="LC604">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-nonterminal</span> <span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC605">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cons</span></div><div class="line" id="LC606">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">case</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC607">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">string</span></div><div class="line" id="LC608">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-string</span> <span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC609">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">and</span></div><div class="line" id="LC610">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-sequence</span> <span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC611">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">or</span></div><div class="line" id="LC612">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-ordered-choise</span> <span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC613">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">*</span></div><div class="line" id="LC614">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-greedy-repetition</span> <span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC615">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">+</span></div><div class="line" id="LC616">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-greedy-positive-repetition</span> <span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC617">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">?</span></div><div class="line" id="LC618">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-optional</span> <span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC619">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">&amp;</span></div><div class="line" id="LC620">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-followed-by</span> <span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC621">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">!</span></div><div class="line" id="LC622">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-not-followed-by</span> <span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC623">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="no">t</span></div><div class="line" id="LC624">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">symbolp</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC625">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">eval-semantic-predicate</span> <span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC626">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">invalid-expression-error</span> <span class="nv">expression</span><span class="p">)))))</span></div><div class="line" id="LC627">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="no">t</span></div><div class="line" id="LC628">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">invalid-expression-error</span> <span class="nv">expression</span><span class="p">))))</span></div><div class="line" id="LC629">&nbsp;</div><div class="line" id="LC630"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-expression</span> <span class="p">(</span><span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC631">&nbsp;&nbsp;<span class="p">(</span><span class="nb">etypecase</span> <span class="nv">expression</span></div><div class="line" id="LC632">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">((</span><span class="nb">eql</span> <span class="nb">character</span><span class="p">)</span></div><div class="line" id="LC633">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">compile-character</span><span class="p">))</span></div><div class="line" id="LC634">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">terminal</span></div><div class="line" id="LC635">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">compile-terminal</span> <span class="p">(</span><span class="nb">string</span> <span class="nv">expression</span><span class="p">)))</span></div><div class="line" id="LC636">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">nonterminal</span></div><div class="line" id="LC637">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">compile-nonterminal</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC638">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">cons</span></div><div class="line" id="LC639">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">case</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC640">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">string</span></div><div class="line" id="LC641">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">compile-string</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC642">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">and</span> </div><div class="line" id="LC643">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">compile-sequence</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC644">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">or</span></div><div class="line" id="LC645">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">compile-ordered-choise</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC646">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">*</span></div><div class="line" id="LC647">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">compile-greedy-repetition</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC648">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">+</span></div><div class="line" id="LC649">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">compile-greedy-positive-repetition</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC650">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">?</span></div><div class="line" id="LC651">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">compile-optional</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC652">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">&amp;</span></div><div class="line" id="LC653">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">compile-followed-by</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC654">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">!</span></div><div class="line" id="LC655">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">compile-not-followed-by</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC656">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="no">t</span></div><div class="line" id="LC657">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">symbolp</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC658">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">compile-semantic-predicate</span> <span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC659">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">invalid-expression-error</span> <span class="nv">expression</span><span class="p">)))))</span></div><div class="line" id="LC660">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="no">t</span></div><div class="line" id="LC661">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">invalid-expression-error</span> <span class="nv">expression</span><span class="p">))))</span></div><div class="line" id="LC662">&nbsp;</div><div class="line" id="LC663"><span class="c1">;;; Characters and strings</span></div><div class="line" id="LC664">&nbsp;</div><div class="line" id="LC665"><span class="p">(</span><span class="nb">declaim</span> <span class="p">(</span><span class="k">inline</span> <span class="nv">exec-string</span><span class="p">))</span></div><div class="line" id="LC666"><span class="p">(</span><span class="nb">defun</span> <span class="nv">exec-string</span> <span class="p">(</span><span class="nb">length</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC667">&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">limit</span> <span class="p">(</span><span class="nb">+</span> <span class="nb">length</span> <span class="nb">position</span><span class="p">)))</span></div><div class="line" id="LC668">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">&lt;=</span> <span class="nv">limit</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC669">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span></div><div class="line" id="LC670">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:production</span> <span class="p">(</span><span class="nb">subseq</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">limit</span><span class="p">)</span></div><div class="line" id="LC671">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nv">limit</span><span class="p">)</span></div><div class="line" id="LC672">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span></div><div class="line" id="LC673">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="ss">&#39;character</span></div><div class="line" id="LC674">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span><span class="p">))))</span></div><div class="line" id="LC675">&nbsp;</div><div class="line" id="LC676"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-character</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC677">&nbsp;&nbsp;<span class="p">(</span><span class="nv">exec-string</span> <span class="mi">1</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC678">&nbsp;</div><div class="line" id="LC679"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-character</span> <span class="p">()</span></div><div class="line" id="LC680">&nbsp;&nbsp;<span class="nf">#&#39;</span><span class="nv">eval-character</span><span class="p">)</span></div><div class="line" id="LC681">&nbsp;</div><div class="line" id="LC682"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-string</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC683">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nb">string</span> <span class="nb">length</span><span class="p">))</span></div><div class="line" id="LC684">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">exec-string</span> <span class="nb">length</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC685">&nbsp;</div><div class="line" id="LC686"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-string</span> <span class="p">(</span><span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC687">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nb">string</span> <span class="nb">length</span><span class="p">))</span></div><div class="line" id="LC688">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">named-lambda</span> <span class="nv">compiled-string</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC689">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">exec-string</span> <span class="nb">length</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))))</span></div><div class="line" id="LC690">&nbsp;</div><div class="line" id="LC691"><span class="c1">;;; Terminals</span></div><div class="line" id="LC692"><span class="c1">;;;</span></div><div class="line" id="LC693"><span class="c1">;;; FIXME: It might be worth it to special-case terminals of length 1.</span></div><div class="line" id="LC694">&nbsp;</div><div class="line" id="LC695"><span class="p">(</span><span class="nb">declaim</span> <span class="p">(</span><span class="k">inline</span> <span class="nv">exec-terminal</span><span class="p">))</span></div><div class="line" id="LC696"><span class="p">(</span><span class="nb">defun</span> <span class="nv">exec-terminal</span> <span class="p">(</span><span class="nb">string</span> <span class="nb">length</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC697">&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">and</span> <span class="p">(</span><span class="nb">&lt;=</span> <span class="p">(</span><span class="nb">+</span> <span class="nb">length</span> <span class="nb">position</span><span class="p">)</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC698">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dotimes</span> <span class="p">(</span><span class="nv">i</span> <span class="nb">length</span> <span class="no">t</span><span class="p">)</span></div><div class="line" id="LC699">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">unless</span> <span class="p">(</span><span class="nb">eql</span> <span class="p">(</span><span class="nb">char</span> <span class="nb">string</span> <span class="nv">i</span><span class="p">)</span> <span class="p">(</span><span class="nb">char</span> <span class="nv">text</span> <span class="p">(</span><span class="nb">+</span> <span class="nv">i</span> <span class="nb">position</span><span class="p">)))</span></div><div class="line" id="LC700">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">return</span> <span class="no">nil</span><span class="p">))))</span></div><div class="line" id="LC701">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span></div><div class="line" id="LC702">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="p">(</span><span class="nb">+</span> <span class="nb">length</span> <span class="nb">position</span><span class="p">)</span></div><div class="line" id="LC703">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:production</span> <span class="nb">string</span><span class="p">)</span></div><div class="line" id="LC704">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span></div><div class="line" id="LC705">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nb">string</span></div><div class="line" id="LC706">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span><span class="p">)))</span></div><div class="line" id="LC707">&nbsp;</div><div class="line" id="LC708"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-terminal</span> <span class="p">(</span><span class="nb">string</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC709">&nbsp;&nbsp;<span class="p">(</span><span class="nv">exec-terminal</span> <span class="nb">string</span> <span class="p">(</span><span class="nb">length</span> <span class="nb">string</span><span class="p">)</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC710">&nbsp;</div><div class="line" id="LC711"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-terminal</span> <span class="p">(</span><span class="nb">string</span><span class="p">)</span></div><div class="line" id="LC712">&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nb">length</span> <span class="p">(</span><span class="nb">length</span> <span class="nb">string</span><span class="p">)))</span></div><div class="line" id="LC713">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">named-lambda</span> <span class="nv">compiled-terminal</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC714">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">exec-terminal</span> <span class="nb">string</span> <span class="nb">length</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))))</span></div><div class="line" id="LC715">&nbsp;</div><div class="line" id="LC716"><span class="c1">;;; Nonterminals</span></div><div class="line" id="LC717">&nbsp;</div><div class="line" id="LC718"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-nonterminal</span> <span class="p">(</span><span class="nc">symbol</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC719">&nbsp;&nbsp;<span class="p">(</span><span class="nb">funcall</span> <span class="p">(</span><span class="nv">cell-function</span> <span class="p">(</span><span class="nv">ensure-rule-cell</span> <span class="nc">symbol</span><span class="p">))</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC720">&nbsp;</div><div class="line" id="LC721"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-nonterminal</span> <span class="p">(</span><span class="nc">symbol</span><span class="p">)</span></div><div class="line" id="LC722">&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">cell</span> <span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">boundp</span> <span class="ss">&#39;*current-rule*</span><span class="p">)</span></div><div class="line" id="LC723">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">reference-rule-cell</span> <span class="nc">symbol</span> <span class="vg">*current-rule*</span><span class="p">)</span></div><div class="line" id="LC724">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">find-rule-cell</span> <span class="nc">symbol</span><span class="p">))))</span></div><div class="line" id="LC725">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">declare</span> <span class="p">(</span><span class="nv">rule-cell</span> <span class="nv">cell</span><span class="p">))</span></div><div class="line" id="LC726">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">named-lambda</span> <span class="nv">compile-nonterminal</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC727">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">funcall</span> <span class="p">(</span><span class="nv">cell-function</span> <span class="nv">cell</span><span class="p">)</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))))</span></div><div class="line" id="LC728">&nbsp;</div><div class="line" id="LC729"><span class="c1">;;; Sequences</span></div><div class="line" id="LC730"><span class="c1">;;;</span></div><div class="line" id="LC731"><span class="c1">;;; FIXME: It might be better if we actually chained the closures</span></div><div class="line" id="LC732"><span class="c1">;;; here, instead of looping over them -- benchmark first, though.</span></div><div class="line" id="LC733">&nbsp;</div><div class="line" id="LC734"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-sequence</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC735">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nb">and</span> <span class="k">&amp;rest</span> <span class="nv">subexprs</span><span class="p">))</span></div><div class="line" id="LC736">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">(</span><span class="nv">results</span><span class="p">)</span></div><div class="line" id="LC737">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dolist</span> <span class="p">(</span><span class="nv">expr</span> <span class="nv">subexprs</span></div><div class="line" id="LC738">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span></div><div class="line" id="LC739">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC740">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:production</span> <span class="p">(</span><span class="nb">mapcar</span> <span class="nf">#&#39;</span><span class="nv">result-production</span> <span class="p">(</span><span class="nb">nreverse</span> <span class="nv">results</span><span class="p">))))</span></div><div class="line" id="LC741">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nv">eval-expression</span> <span class="nv">expr</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC742">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC743">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">return</span> <span class="p">(</span><span class="nv">make-error-result</span></div><div class="line" id="LC744">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span></div><div class="line" id="LC745">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC746">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:detail</span> <span class="nv">result</span><span class="p">))</span></div><div class="line" id="LC747">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="nb">position</span> <span class="p">(</span><span class="nv">result-position</span> <span class="nv">result</span><span class="p">)))</span></div><div class="line" id="LC748">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">push</span> <span class="nv">result</span> <span class="nv">results</span><span class="p">))))))</span></div><div class="line" id="LC749">&nbsp;</div><div class="line" id="LC750"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-sequence</span> <span class="p">(</span><span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC751">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nb">and</span> <span class="k">&amp;rest</span> <span class="nv">subexprs</span><span class="p">))</span></div><div class="line" id="LC752">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">functions</span> <span class="p">(</span><span class="nb">mapcar</span> <span class="nf">#&#39;</span><span class="nv">compile-expression</span> <span class="nv">subexprs</span><span class="p">)))</span></div><div class="line" id="LC753">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">named-lambda</span> <span class="nv">compiled-sequence</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC754">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">(</span><span class="nv">results</span><span class="p">)</span></div><div class="line" id="LC755">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dolist</span> <span class="p">(</span><span class="nv">fun</span> <span class="nv">functions</span> </div><div class="line" id="LC756">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span></div><div class="line" id="LC757">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC758">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:production</span> <span class="p">(</span><span class="nb">mapcar</span> <span class="nf">#&#39;</span><span class="nv">result-production</span> <span class="p">(</span><span class="nb">nreverse</span> <span class="nv">results</span><span class="p">))))</span></div><div class="line" id="LC759">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nb">funcall</span> <span class="nv">fun</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC760">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC761">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">return</span> <span class="p">(</span><span class="nv">make-error-result</span> </div><div class="line" id="LC762">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span></div><div class="line" id="LC763">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC764">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:detail</span> <span class="nv">result</span><span class="p">))</span></div><div class="line" id="LC765">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="nb">position</span> <span class="p">(</span><span class="nv">result-position</span> <span class="nv">result</span><span class="p">)))</span></div><div class="line" id="LC766">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">push</span> <span class="nv">result</span> <span class="nv">results</span><span class="p">))))))))</span></div><div class="line" id="LC767">&nbsp;</div><div class="line" id="LC768"><span class="c1">;;; Ordered choises</span></div><div class="line" id="LC769">&nbsp;</div><div class="line" id="LC770"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-ordered-choise</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC771">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nb">or</span> <span class="k">&amp;rest</span> <span class="nv">subexprs</span><span class="p">))</span></div><div class="line" id="LC772">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">(</span><span class="nv">last-error</span><span class="p">)</span></div><div class="line" id="LC773">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dolist</span> <span class="p">(</span><span class="nv">expr</span> <span class="nv">subexprs</span></div><div class="line" id="LC774">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span> </div><div class="line" id="LC775">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span></div><div class="line" id="LC776">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC777">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:detail</span> <span class="nv">last-error</span><span class="p">))</span></div><div class="line" id="LC778">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nv">eval-expression</span> <span class="nv">expr</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC779">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC780">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="p">(</span><span class="nb">or</span> <span class="p">(</span><span class="nb">not</span> <span class="nv">last-error</span><span class="p">)</span></div><div class="line" id="LC781">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">&lt;</span> <span class="p">(</span><span class="nv">error-result-position</span> <span class="nv">last-error</span><span class="p">)</span> </div><div class="line" id="LC782">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">error-result-position</span> <span class="nv">result</span><span class="p">)))</span></div><div class="line" id="LC783">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="nv">last-error</span> <span class="nv">result</span><span class="p">))</span></div><div class="line" id="LC784">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">return</span> <span class="nv">result</span><span class="p">)))))))</span></div><div class="line" id="LC785">&nbsp;</div><div class="line" id="LC786"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-ordered-choise</span> <span class="p">(</span><span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC787">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nb">or</span> <span class="k">&amp;rest</span> <span class="nv">subexprs</span><span class="p">))</span></div><div class="line" id="LC788">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">functions</span> <span class="p">(</span><span class="nb">mapcar</span> <span class="nf">#&#39;</span><span class="nv">compile-expression</span> <span class="nv">subexprs</span><span class="p">)))</span></div><div class="line" id="LC789">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">named-lambda</span> <span class="nv">compiled-ordered-choise</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC790">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">(</span><span class="nv">last-error</span><span class="p">)</span></div><div class="line" id="LC791">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dolist</span> <span class="p">(</span><span class="nv">fun</span> <span class="nv">functions</span> </div><div class="line" id="LC792">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span> </div><div class="line" id="LC793">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span></div><div class="line" id="LC794">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC795">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:detail</span> <span class="nv">last-error</span><span class="p">))</span></div><div class="line" id="LC796">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nb">funcall</span> <span class="nv">fun</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC797">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC798">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">when</span> <span class="p">(</span><span class="nb">or</span> <span class="p">(</span><span class="nb">not</span> <span class="nv">last-error</span><span class="p">)</span> </div><div class="line" id="LC799">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">&lt;</span> <span class="p">(</span><span class="nv">error-result-position</span> <span class="nv">last-error</span><span class="p">)</span> </div><div class="line" id="LC800">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">error-result-position</span> <span class="nv">result</span><span class="p">)))</span></div><div class="line" id="LC801">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="nv">last-error</span> <span class="nv">result</span><span class="p">))</span></div><div class="line" id="LC802">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">return</span> <span class="nv">result</span><span class="p">)))))))))</span></div><div class="line" id="LC803">&nbsp;</div><div class="line" id="LC804"><span class="c1">;;; Greedy repetitions</span></div><div class="line" id="LC805">&nbsp;</div><div class="line" id="LC806"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-greedy-repetition</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC807">&nbsp;&nbsp;<span class="p">(</span><span class="nb">funcall</span> <span class="p">(</span><span class="nv">compile-greedy-repetition</span> <span class="nv">expression</span><span class="p">)</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC808">&nbsp;</div><div class="line" id="LC809"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-greedy-repetition</span> <span class="p">(</span><span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC810">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nb">*</span> <span class="nv">subexpr</span><span class="p">))</span></div><div class="line" id="LC811">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="k">function</span> <span class="p">(</span><span class="nv">compile-expression</span> <span class="nv">subexpr</span><span class="p">)))</span></div><div class="line" id="LC812">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">named-lambda</span> <span class="nv">compiled-greedy-repetition</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC813">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">results</span> </div><div class="line" id="LC814">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">loop</span> <span class="nv">for</span> <span class="nv">result</span> <span class="nb">=</span> <span class="p">(</span><span class="nb">funcall</span> <span class="k">function</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC815">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">until</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC816">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nb">do</span> <span class="p">(</span><span class="nb">setf</span> <span class="nb">position</span> <span class="p">(</span><span class="nv">result-position</span> <span class="nv">result</span><span class="p">))</span></div><div class="line" id="LC817">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">collect</span> <span class="nv">result</span><span class="p">)))</span></div><div class="line" id="LC818">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span></div><div class="line" id="LC819">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC820">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:production</span> <span class="p">(</span><span class="nb">mapcar</span> <span class="nf">#&#39;</span><span class="nv">result-production</span> <span class="nv">results</span><span class="p">)))))))</span></div><div class="line" id="LC821">&nbsp;</div><div class="line" id="LC822"><span class="c1">;;; Greedy positive repetitions</span></div><div class="line" id="LC823">&nbsp;</div><div class="line" id="LC824"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-greedy-positive-repetition</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC825">&nbsp;&nbsp;<span class="p">(</span><span class="nb">funcall</span> <span class="p">(</span><span class="nv">compile-greedy-positive-repetition</span> <span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC826">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">))</span></div><div class="line" id="LC827">&nbsp;</div><div class="line" id="LC828"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-greedy-positive-repetition</span> <span class="p">(</span><span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC829">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nb">+</span> <span class="nv">subexpr</span><span class="p">))</span></div><div class="line" id="LC830">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="k">function</span> <span class="p">(</span><span class="nv">compile-expression</span> <span class="nv">subexpr</span><span class="p">)))</span></div><div class="line" id="LC831">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">named-lambda</span> <span class="nv">compiled-greedy-positive-repetition</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC832">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let*</span> <span class="p">((</span><span class="nb">last</span> <span class="no">nil</span><span class="p">)</span></div><div class="line" id="LC833">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">results</span></div><div class="line" id="LC834">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">loop</span> <span class="nv">for</span> <span class="nv">result</span> <span class="nb">=</span> <span class="p">(</span><span class="nb">funcall</span> <span class="k">function</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC835">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">until</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="p">(</span><span class="nb">setf</span> <span class="nb">last</span> <span class="nv">result</span><span class="p">))</span></div><div class="line" id="LC836">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nb">do</span> <span class="p">(</span><span class="nb">setf</span> <span class="nb">position</span> <span class="p">(</span><span class="nv">result-position</span> <span class="nv">result</span><span class="p">))</span></div><div class="line" id="LC837">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">collect</span> <span class="nv">result</span><span class="p">)))</span></div><div class="line" id="LC838">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="nv">results</span></div><div class="line" id="LC839">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span></div><div class="line" id="LC840">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC841">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:production</span> <span class="p">(</span><span class="nb">mapcar</span> <span class="nf">#&#39;</span><span class="nv">result-production</span> <span class="nv">results</span><span class="p">))</span></div><div class="line" id="LC842">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span></div><div class="line" id="LC843">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC844">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span></div><div class="line" id="LC845">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:detail</span> <span class="nb">last</span><span class="p">)))))))</span></div><div class="line" id="LC846">&nbsp;</div><div class="line" id="LC847"><span class="c1">;;; Optionals</span></div><div class="line" id="LC848">&nbsp;</div><div class="line" id="LC849"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-optional</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC850">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nv">?</span> <span class="nv">subexpr</span><span class="p">))</span></div><div class="line" id="LC851">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nv">eval-expression</span> <span class="nv">subexpr</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC852">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC853">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span> <span class="ss">:position</span> <span class="nb">position</span><span class="p">)</span></div><div class="line" id="LC854">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">result</span><span class="p">))))</span></div><div class="line" id="LC855">&nbsp;</div><div class="line" id="LC856"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-optional</span> <span class="p">(</span><span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC857">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nv">?</span> <span class="nv">subexpr</span><span class="p">))</span></div><div class="line" id="LC858">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="k">function</span> <span class="p">(</span><span class="nv">compile-expression</span> <span class="nv">subexpr</span><span class="p">)))</span></div><div class="line" id="LC859">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">named-lambda</span> <span class="nv">compiled-optional</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC860">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nb">funcall</span> <span class="k">function</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC861">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC862">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span> <span class="ss">:position</span> <span class="nb">position</span><span class="p">)</span></div><div class="line" id="LC863">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">result</span><span class="p">))))))</span></div><div class="line" id="LC864">&nbsp;</div><div class="line" id="LC865"><span class="c1">;;; Followed-by&#39;s</span></div><div class="line" id="LC866">&nbsp;</div><div class="line" id="LC867"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-followed-by</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC868">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nv">&amp;</span> <span class="nv">subexpr</span><span class="p">))</span></div><div class="line" id="LC869">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nv">eval-expression</span> <span class="nv">subexpr</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC870">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC871">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span> </div><div class="line" id="LC872">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC873">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span></div><div class="line" id="LC874">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:detail</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC875">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span></div><div class="line" id="LC876">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC877">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:production</span> <span class="p">(</span><span class="nv">result-production</span> <span class="nv">result</span><span class="p">))))))</span></div><div class="line" id="LC878">&nbsp;</div><div class="line" id="LC879"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-followed-by</span> <span class="p">(</span><span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC880">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nv">&amp;</span> <span class="nv">subexpr</span><span class="p">))</span></div><div class="line" id="LC881">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="k">function</span> <span class="p">(</span><span class="nv">compile-expression</span> <span class="nv">subexpr</span><span class="p">)))</span></div><div class="line" id="LC882">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">named-lambda</span> <span class="nv">compiled-followed-by</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC883">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nb">funcall</span> <span class="k">function</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC884">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC885">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span></div><div class="line" id="LC886">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC887">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span></div><div class="line" id="LC888">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:detail</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC889">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span></div><div class="line" id="LC890">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC891">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:production</span> <span class="p">(</span><span class="nv">result-production</span> <span class="nv">result</span><span class="p">))))))))</span></div><div class="line" id="LC892">&nbsp;</div><div class="line" id="LC893"><span class="c1">;;; Not followed-by&#39;s</span></div><div class="line" id="LC894">&nbsp;</div><div class="line" id="LC895"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-not-followed-by</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC896">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nv">!</span> <span class="nv">subexpr</span><span class="p">))</span></div><div class="line" id="LC897">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nv">eval-expression</span> <span class="nv">subexpr</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC898">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC899">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span> </div><div class="line" id="LC900">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span><span class="p">)</span></div><div class="line" id="LC901">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span></div><div class="line" id="LC902">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span></div><div class="line" id="LC903">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span><span class="p">)))))</span></div><div class="line" id="LC904">&nbsp;</div><div class="line" id="LC905"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-not-followed-by</span> <span class="p">(</span><span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC906">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="nv">!</span> <span class="nv">subexpr</span><span class="p">))</span></div><div class="line" id="LC907">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="k">function</span> <span class="p">(</span><span class="nv">compile-expression</span> <span class="nv">subexpr</span><span class="p">)))</span></div><div class="line" id="LC908">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">named-lambda</span> <span class="nv">compiled-not-followed-by</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC909">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nb">funcall</span> <span class="k">function</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC910">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC911">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-result</span></div><div class="line" id="LC912">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span><span class="p">)</span></div><div class="line" id="LC913">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span></div><div class="line" id="LC914">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span></div><div class="line" id="LC915">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span><span class="p">)))))))</span></div><div class="line" id="LC916">&nbsp;</div><div class="line" id="LC917"><span class="c1">;;; Semantic predicates</span></div><div class="line" id="LC918">&nbsp;</div><div class="line" id="LC919"><span class="p">(</span><span class="nb">defun</span> <span class="nv">eval-semantic-predicate</span> <span class="p">(</span><span class="nv">expression</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC920">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="no">t</span> <span class="nv">subexpr</span><span class="p">))</span></div><div class="line" id="LC921">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nv">eval-expression</span> <span class="nv">subexpr</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC922">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC923">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span></div><div class="line" id="LC924">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC925">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span></div><div class="line" id="LC926">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:detail</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC927">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">production</span> <span class="p">(</span><span class="nv">result-production</span> <span class="nv">result</span><span class="p">)))</span></div><div class="line" id="LC928">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">funcall</span> <span class="p">(</span><span class="nb">symbol-function</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">expression</span><span class="p">))</span> <span class="nv">production</span><span class="p">)</span></div><div class="line" id="LC929">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">result</span></div><div class="line" id="LC930">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span> </div><div class="line" id="LC931">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC932">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span><span class="p">)))))))</span></div><div class="line" id="LC933">&nbsp;</div><div class="line" id="LC934"><span class="p">(</span><span class="nb">defun</span> <span class="nv">compile-semantic-predicate</span> <span class="p">(</span><span class="nv">expression</span><span class="p">)</span></div><div class="line" id="LC935">&nbsp;&nbsp;<span class="p">(</span><span class="nv">with-expression</span> <span class="p">(</span><span class="nv">expression</span> <span class="p">(</span><span class="no">t</span> <span class="nv">subexpr</span><span class="p">))</span></div><div class="line" id="LC936">&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let*</span> <span class="p">((</span><span class="k">function</span> <span class="p">(</span><span class="nv">compile-expression</span> <span class="nv">subexpr</span><span class="p">))</span></div><div class="line" id="LC937">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">predicate</span> <span class="p">(</span><span class="nb">car</span> <span class="nv">expression</span><span class="p">))</span></div><div class="line" id="LC938">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; KLUDGE: Calling via a variable symbol can be slow, and if we</span></div><div class="line" id="LC939">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">;; grab the SYMBOL-FUNCTION here we will not see redefinitions.</span></div><div class="line" id="LC940">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">semantic-function</span> </div><div class="line" id="LC941">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">eq</span> <span class="p">(</span><span class="nb">symbol-package</span> <span class="nv">predicate</span><span class="p">)</span> <span class="p">(</span><span class="k">load-time-value</span> <span class="p">(</span><span class="nb">find-package</span> <span class="ss">:cl</span><span class="p">)))</span></div><div class="line" id="LC942">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">symbol-function</span> <span class="nv">predicate</span><span class="p">)</span></div><div class="line" id="LC943">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">compile</span> <span class="no">nil</span> <span class="o">`</span><span class="p">(</span><span class="k">lambda</span> <span class="p">(</span><span class="nv">x</span><span class="p">)</span> <span class="p">(</span><span class="o">,</span><span class="nv">predicate</span> <span class="nv">x</span><span class="p">))))))</span></div><div class="line" id="LC944">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">named-lambda</span> <span class="nv">compiled-semantic-predicate</span> <span class="p">(</span><span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)</span></div><div class="line" id="LC945">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="nb">funcall</span> <span class="k">function</span> <span class="nv">text</span> <span class="nb">position</span> <span class="nv">end</span><span class="p">)))</span></div><div class="line" id="LC946">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nv">error-result-p</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC947">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span></div><div class="line" id="LC948">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC949">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span></div><div class="line" id="LC950">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:detail</span> <span class="nv">result</span><span class="p">)</span></div><div class="line" id="LC951">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">production</span> <span class="p">(</span><span class="nv">result-production</span> <span class="nv">result</span><span class="p">)))</span></div><div class="line" id="LC952">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">funcall</span> <span class="nv">semantic-function</span> <span class="nv">production</span><span class="p">)</span></div><div class="line" id="LC953">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">result</span></div><div class="line" id="LC954">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">make-error-result</span></div><div class="line" id="LC955">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:position</span> <span class="nb">position</span></div><div class="line" id="LC956">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="ss">:expression</span> <span class="nv">expression</span><span class="p">)))))))))</span></div><div class="line" id="LC957">&nbsp;</div></pre></div>
            
          </td>
        </tr>
      </table>
    
  </div>


      </div>
    </div>

  


    </div>
  
      

      <div class="push"></div>
    </div>

    <div id="footer">
      <div class="site">
        <div class="info">
          <div class="links">
            <a href="http://github.com/blog"><b>Blog</b></a> |
            <a href="http://support.github.com/">Support</a> |
            <a href="http://github.com/training">Training</a> |
            <a href="http://github.com/contact">Contact</a> |
            <a href="http://develop.github.com">API</a> |
            <a href="http://status.github.com">Status</a> |
            <a href="http://twitter.com/github">Twitter</a> |
            <a href="http://help.github.com">Help</a> |
            <a href="http://github.com/security">Security</a>
          </div>
          <div class="company">
            &copy;
            2010
            <span id="_rrt" title="0.06674s from fe1.rs.github.com">GitHub</span> Inc.
            All rights reserved. |
            <a href="/site/terms">Terms of Service</a> |
            <a href="/site/privacy">Privacy Policy</a>
          </div>
        </div>
        <div class="sponsor">
          <div>
            Powered by the <a href="http://www.rackspace.com ">Dedicated
            Servers</a> and<br/> <a href="http://www.rackspacecloud.com">Cloud
            Computing</a> of Rackspace Hosting<span>&reg;</span>
          </div>
          <a href="http://www.rackspace.com">
            <img alt="Dedicated Server" src="http://assets3.github.com/images/modules/footer/rackspace_logo.png?7371c81fbc6b010a32fb11b42a0fc322c3c57863" />
          </a>
        </div>
      </div>
    </div>

    <script>window._auth_token = "2d10bc5ead7c36efc020ba2c079560fb0a138d02"</script>
    
    
  </body>
</html>

