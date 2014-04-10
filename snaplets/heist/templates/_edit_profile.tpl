<apply template="base">

  <dfForm>
    <dfChildErrorList ref=""/>
    <div class="nameAndPic">
      <userName/>
      <img src="${userPicPath}" style="float:left;"/>
      Profile picture. Upload: <dfInputFile type="file" ref="profilePicFile"/> or URL:
    </div>

    <div class="personalInfo">
      Real Name: <dfInput type="text" size="20" class="form-control" value="${realName}" ref="realName"/>
      Website: <dfInput type="text" size="20" class="form-control" value="${website}" ref="website"/>
    </div>

    <div class="personalDescription">
      About you:
      <dfInputTextArea rows="10" cols="30" value="${personalDescription}" ref="userInfoText"/>
    </div>

    <dfInputSubmit value="Update" class="btn btn-default"/>

  </dfForm>

</apply>
