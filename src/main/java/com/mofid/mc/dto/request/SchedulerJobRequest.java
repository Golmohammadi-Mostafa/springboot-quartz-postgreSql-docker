package com.mofid.mc.dto.request;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SchedulerJobRequest implements Serializable {

    private String jobName;
    private String jobGroup;
    private String jobClass;
    private String cronExpression;
    private Date jobScheduleTime;
    private Boolean cronJob;
    private Boolean enabled;
    private Long repeatTime;
    private String jobStatus;
}
